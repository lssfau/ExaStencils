package exastencils.optimization

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

import exastencils.core.Duplicate
import exastencils.cuda
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge.Knowledge
import exastencils.knowledge.Platform
import exastencils.logger.Logger
import exastencils.performance.SIMD_MathFunctions
import exastencils.strategies.SimplifyStrategy
import exastencils.util.SimplifyExpression

object Vectorization extends DefaultStrategy("Vectorization") {

  final val VECT_ANNOT : String = "VECT"
  final val COND_VECTABLE : String = "VECT_C"
  final val COND_IGN_INCR : String = "VECT_ign++"

  this += new Transformation("optimize", VectorizeInnermost, false)
}

final class VectorizationException(val msg : String) extends Exception(msg)

private object VectorizeInnermost extends PartialFunction[Node, Transformation.OutputType] {

  private val DEBUG : Boolean = false

  private var skipSubTree : Boolean = false

  override def isDefinedAt(node : Node) : Boolean = {
    // do not vectorize device code!
    node match {
      case _ : cuda.Kernel               => skipSubTree = true
      case _ : AbstractFunctionStatement => skipSubTree = false
      case _                             => // no change in skipSubTree
    }
    if (skipSubTree)
      return false

    node.removeAnnotation(AddressPrecalculation.ORIG_IND_ANNOT) // remove old annotations
    return node match {
      case loop : ForLoopStatement with OptimizationHint =>
        loop.isInnermost && (loop.isParallel || loop.isVectorizable) && !loop.hasAnnotation(Vectorization.VECT_ANNOT)
      case _ =>
        false
    }
  }

  override def apply(node : Node) : Transformation.OutputType = {
    return try {
      vectorizeLoop(node.asInstanceOf[ForLoopStatement])
    } catch {
      case ex : VectorizationException =>
        if (DEBUG)
          Logger.debug("[vect]  unable to vectorize loop: " + ex.msg + "  (line " + ex.getStackTrace()(0).getLineNumber + ')')
        node
    }
  }

  private def vectorizeLoop(loop : ForLoopStatement) : Statement = {

    // excessive testing if loop header allows vectorization
    return loop match {
      case ForLoopStatement(VariableDeclarationStatement(IntegerDatatype, itName, Some(lBound)), condExpr, incrExpr, body, reduction) =>

        val uBoundExcl : Expression =
          condExpr match {
            case LowerExpression(VariableAccess(bName, Some(IntegerDatatype)), upperBoundExcl) if (itName == bName) =>
              upperBoundExcl
            case LowerEqualExpression(VariableAccess(bName, Some(IntegerDatatype)), upperBoundIncl) if (itName == bName) =>
              new AdditionExpression(upperBoundIncl, IntegerConstant(1))
            case _ => throw new VectorizationException("no upper bound")
          }

        val incr : Long =
          incrExpr match {
            case ExpressionStatement(PreIncrementExpression(VariableAccess(n, Some(IntegerDatatype)))) if (itName == n)  => 1L
            case ExpressionStatement(PostIncrementExpression(VariableAccess(n, Some(IntegerDatatype)))) if (itName == n) => 1L
            case AssignmentStatement(VariableAccess(n, Some(IntegerDatatype)),
              IntegerConstant(i),
              "+=") if (itName == n) => i
            case AssignmentStatement(VariableAccess(n1, Some(IntegerDatatype)),
              AdditionExpression(ListBuffer(IntegerConstant(i), VariableAccess(n2, Some(IntegerDatatype)))),
              "=") if (itName == n1 && itName == n2) => i
            case AssignmentStatement(VariableAccess(n1, Some(IntegerDatatype)),
              AdditionExpression(ListBuffer(VariableAccess(n2, Some(IntegerDatatype)), IntegerConstant(i))),
              "=") if (itName == n1 && itName == n2) => i
            case _ => throw new VectorizationException("loop increment must be constant or cannot be extracted:  " + incrExpr)
          }

        vectorizeLoop(Duplicate(loop), itName, lBound, uBoundExcl, incr, body, reduction)

      case _ => throw new VectorizationException("cannot analyze loop (yet)")
    }
  }

  private final class LoopCtx(val itName : String, val incr : Long) {

    private val vectStmtsStack = new ArrayBuffer[ListBuffer[Statement]]()
    var storesTmp : Statement = null
    var ignIncr : Boolean = false

    private val preLoopStmts = new ListBuffer[Statement]()
    private val postLoopStmts = new ListBuffer[Statement]()

    val toFinish_LCSE = Map[String, SIMD_ConcShift]()

    private val temporaryMappingStack = new ArrayBuffer[Map[Expression, String]]()
    private val temporaryProperties = Map[String, (Boolean, Boolean)]()
    private var isStore_ : Boolean = false
    private var varID : Int = -1
    private var incrVectDeclared : Boolean = false
    private var alignedResidue : Long = -1
    private val nameTempl : String = "_vec%02d"

    // init
    pushScope()

    def addStmt(stmt : Statement) : Unit = {
      vectStmtsStack.last += stmt
    }

    def addStmtPreLoop(stmt : Statement) : Unit = {
      if (stmt.isInstanceOf[VariableDeclarationStatement])
        throw new Error("use addStmtPreLoop(VariableDeclarationStatement, Expression) instead of addStmtPreLoop(Statement)!")
      preLoopStmts += stmt
    }

    def addStmtPreLoop(decl : VariableDeclarationStatement, origExpr : Expression) : Unit = {
      moveNameMappingUp(origExpr)
      preLoopStmts += decl
    }

    def getPreLoopStmts() : ListBuffer[Statement] = {
      return preLoopStmts
    }

    def addStmtPostLoop(stmt : Statement) : Unit = {
      postLoopStmts += stmt
    }

    def getPostLoopStmts() : ListBuffer[Statement] = {
      return postLoopStmts
    }

    def pushScope() : Unit = {
      temporaryMappingStack += Map[Expression, String]()
      vectStmtsStack += new ListBuffer[Statement]()
    }

    def popScope() : ListBuffer[Statement] = {
      temporaryMappingStack.remove(temporaryMappingStack.length - 1)
      return vectStmtsStack.remove(vectStmtsStack.length - 1)
    }

    def getName(expr : Expression) : (String, Boolean) = {
      var i = temporaryMappingStack.length - 1
      while (i >= 0) {
        val nameOpt : Option[String] = temporaryMappingStack(i).get(expr)
        if (nameOpt.isDefined)
          return (nameOpt.get, false)
        i -= 1
      }
      varID += 1
      val name : String = nameTempl.format(varID)
      temporaryMappingStack.last.update(expr, name)
      return (name, true)
    }

    def moveNameMappingUp(expr : Expression) : Unit = {
      temporaryMappingStack(0)(expr) = temporaryMappingStack(temporaryMappingStack.length - 1).remove(expr).get
    }

    def getIncrVector() : VariableAccess = {
      val name : String = "_veci"
      if (!incrVectDeclared) {
        SIMD_IncrementVectorDeclaration(name, incr) +=: preLoopStmts
        incrVectDeclared = true
      }
      return new VariableAccess(name, SIMD_RealDatatype)
    }

    def setLoad() : this.type = {
      isStore_ = false
      return this
    }

    def setStore() : this.type = {
      isStore_ = true
      return this
    }

    def isLoad() : Boolean = {
      return !isStore_
    }

    def isStore() : Boolean = {
      return isStore_
    }

    def setAlignAndAccess1(tmp : String, alignment : Boolean, access1 : Boolean) : Unit = {
      temporaryProperties(tmp) = (alignment, access1)
    }

    def getAlignAndAccess1(tmp : String) : (Boolean, Boolean) = {
      return temporaryProperties(tmp)
    }

    def setAlignedResidue(residue : Long) : Unit = {
      if (alignedResidue < 0)
        alignedResidue = residue
      else if (alignedResidue != residue)
        throw new VectorizationException("Stores with different alignment are not allowed.")
    }

    def getAlignedResidue() : Long = {
      return alignedResidue
    }
  }

  private def containsVarAcc(node : Node, varName : String) : Boolean = {
    var found = false
    val search = new QuietDefaultStrategy("Find VariableAccess...")
    search += new Transformation("seaching...", {
      case acc @ VariableAccess(name, _) if (name == varName) =>
        found = true
        acc
    })
    // ensure node itself is found, too
    search.applyStandalone(Root(ListBuffer(node)))
    return found
  }

  private def vectorizeLoop(oldLoop : ForLoopStatement, itVar : String, begin : Expression, endExcl : Expression,
      incr : Long, body : ListBuffer[Statement], reduction : Option[Reduction]) : Statement = {

    val ctx = new LoopCtx(itVar, incr)
    var postLoopStmt : Statement = null
    if (reduction.isDefined) {
      val target = reduction.get.target
      val operator = reduction.get.op

      val (vecTmp : String, true) = ctx.getName(target)
      val identityElem : Expression =
        operator match {
          case "+"   => SIMD_Scalar2VectorExpression(FloatConstant(0.0))
          case "*"   => SIMD_Scalar2VectorExpression(FloatConstant(1.0))
          case "min" => SIMD_Scalar2VectorExpression(FloatConstant(Double.MaxValue))
          case "max" => SIMD_Scalar2VectorExpression(FloatConstant(Double.MinValue))
          case _     => throw new VectorizationException("unknown reduction operator:  " + operator)
        }
      ctx.addStmtPreLoop(new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, identityElem), target)

      val vecTmpAcc = new VariableAccess(vecTmp, SIMD_RealDatatype)
      postLoopStmt =
        operator match {
          case "+"   => SIMD_HorizontalAddStatement(Duplicate(target), vecTmpAcc)
          case "*"   => SIMD_HorizontalMulStatement(Duplicate(target), vecTmpAcc)
          case "min" => SIMD_HorizontalMinStatement(Duplicate(target), vecTmpAcc)
          case "max" => SIMD_HorizontalMaxStatement(Duplicate(target), vecTmpAcc)
        }
    }

    // ensure all stores are aligned (heuristics)
    var alignmentExpr : Expression = null
    val vs = Platform.simd_vectorSize
    if (Knowledge.data_alignFieldPointers) {
      for (stmt <- body)
        stmt match {
          case AssignmentStatement(acc @ ArrayAccess(_, index, true), _, _) =>
            val annot = acc.getAnnotation(AddressPrecalculation.ORIG_IND_ANNOT)
            val ind : Expression = if (annot.isDefined) annot.get.asInstanceOf[Expression] else index
            val const : Long = SimplifyExpression.extractIntegralSum(ind).getOrElse(SimplifyExpression.constName, 0L)
            val residue : Long = (const % vs + vs) % vs
            ctx.setAlignedResidue(residue)
            alignmentExpr = ind
          case _ =>
        }

      val indexExprs = new ListBuffer[HashMap[Expression, Long]]()
      val collectIndexExprs = new QuietDefaultStrategy("Collect all array index expressions...")
      collectIndexExprs += new Transformation("seaching...", {
        case acc @ ArrayAccess(_, index, true) =>
          if (containsVarAcc(index, ctx.itName)) {
            val annot = acc.removeAnnotation(AddressPrecalculation.ORIG_IND_ANNOT)
            indexExprs += SimplifyExpression.extractIntegralSum(if (annot.isDefined) annot.get.asInstanceOf[Expression] else index)
          }
          acc
      })
      collectIndexExprs.applyStandalone(body)

      // no store available, so align as many loads as possible
      if (alignmentExpr == null) {
        alignmentExpr = SimplifyExpression.recreateExprFromIntSum(indexExprs.head)
        val counts = new Array[Long](vs)
        for (ind <- indexExprs) {
          val const : Long = ind.remove(SimplifyExpression.constName).getOrElse(0L)
          val residue : Long = (const % vs + vs) % vs
          counts(residue.toInt) += 1
        }
        var max = (0, counts(0))
        for (i <- 1 until vs)
          if (counts(i) > max._2)
            max = (i, counts(i))
        ctx.setAlignedResidue(max._1)

      } else
        for (ind <- indexExprs)
          ind.remove(SimplifyExpression.constName)

      // check if index expressions are "good", i.e., all (except the constant summand) have the same residue
      while (!indexExprs.head.isEmpty) {
        val key : Expression = indexExprs.head.head._1
        var residue : Long = -1
        for (ind <- indexExprs) {
          val res = (ind.remove(key).getOrElse(0L) % vs + vs) % vs
          if (residue < 0)
            residue = res
          else if (res != residue)
            throw new VectorizationException("Cannot determine alignment properly")
        }
      }
      // at least one sum is empty, so all remaining coefficients must be evenly dividable
      for (ind <- indexExprs)
        for ((_, coeff) <- ind)
          if (coeff % vs != 0)
            throw new VectorizationException("Cannot determine alignment properly")
    }

    for (stmt <- body)
      vectorizeStmt(stmt, ctx)

    def itVarAcc = new VariableAccess(itVar, IntegerDatatype)
    val newIncr : Long = incr * vs

    oldLoop.begin = new VariableDeclarationStatement(IntegerDatatype, itVar, Unrolling.startVarAcc)
    oldLoop.end = new LowerExpression(itVarAcc, Unrolling.intermVarAcc)
    oldLoop.inc = new AssignmentStatement(itVarAcc, IntegerConstant(newIncr), "+=")
    oldLoop.body = ctx.popScope()

    var postLoop : Statement = null
    val annot = oldLoop.removeAnnotation(Unrolling.UNROLLED_ANNOT)
    val unrolled : Boolean = annot.isDefined
    var res : ListBuffer[Statement] = null
    if (unrolled) {
      res = new ListBuffer[Statement]()
    } else {
      // old AST will be replaced completely, so we can reuse the body once here (and duplicate later)
      val (boundsDecls, postLoop_) : (ListBuffer[Statement], Statement) =
        Unrolling.getBoundsDeclAndPostLoop(itVar, begin, endExcl, incr, body, Duplicate(reduction))
      postLoop = postLoop_
      res = boundsDecls
    }

    if (Knowledge.data_alignFieldPointers) { // no need to ensure alignment of iteration variable if data is not aligned
      val preEndVar : String = "_preEnd"
      def preEndVarAcc = new VariableAccess(preEndVar, IntegerDatatype)

      val wrappedAlignExpr = new ExpressionStatement(Duplicate(alignmentExpr))
      val replItVar = new QuietDefaultStrategy("Replace iteration variable...")
      replItVar += new Transformation("seaching...", {
        case VariableAccess(name, _) if (name == itVar) =>
          Unrolling.startVarAcc
      })
      // ensure node itself is found, too
      replItVar.applyStandalone(wrappedAlignExpr)
      val preEndExpr = new MinimumExpression(Unrolling.endVarAcc,
        Unrolling.startVarAcc + ((IntegerConstant(vs) - (wrappedAlignExpr.expression Mod IntegerConstant(vs))) Mod IntegerConstant(vs)))
      res += new VariableDeclarationStatement(IntegerDatatype, preEndVar, preEndExpr)

      res += new ForLoopStatement(new VariableDeclarationStatement(IntegerDatatype, itVar, Unrolling.startVarAcc),
        new LowerExpression(itVarAcc, preEndVarAcc),
        new AssignmentStatement(itVarAcc, IntegerConstant(incr), "+="),
        Duplicate(body))

      res += new AssignmentStatement(Unrolling.startVarAcc, preEndVarAcc, "=")
    }
    var intermDecl : VariableDeclarationStatement = null
    if (unrolled) {
      intermDecl = annot.get.asInstanceOf[VariableDeclarationStatement]
      intermDecl.expression = Some(Unrolling.getIntermExpr(newIncr))
    } else {
      intermDecl = Unrolling.getIntermDecl(newIncr)
      res += intermDecl
    }

    val emptyLoopGuard = new ConditionStatement(LowerExpression(Unrolling.startVarAcc, Unrolling.intermVarAcc), new ListBuffer[Statement]())
    emptyLoopGuard.trueBody ++= ctx.getPreLoopStmts()
    emptyLoopGuard.trueBody += oldLoop
    emptyLoopGuard.trueBody ++= ctx.getPostLoopStmts()
    if (postLoopStmt != null)
      emptyLoopGuard.trueBody += postLoopStmt
    res += emptyLoopGuard
    if (!unrolled)
      res += postLoop

    oldLoop.annotate(Vectorization.VECT_ANNOT)
    oldLoop.annotate(Unrolling.UNROLLED_ANNOT, intermDecl)
    return new Scope(res)
  }

  private def vectorizeStmt(stmt : Statement, ctx : LoopCtx) : Unit = {
    stmt match {
      case NullStatement => // SIMD_NullStatement? no way...

      case CommentStatement(str) =>
        ctx.addStmt(new CommentStatement(str)) // new instance

      case AssignmentStatement(lhsSca, rhsSca, assOp) =>
        ctx.addStmt(new CommentStatement(stmt.prettyprint()))
        val srcWrap = new ExpressionStatement(Duplicate(assOp match {
          case "="  => rhsSca
          case "+=" => new AdditionExpression(lhsSca, rhsSca)
          case "-=" => new SubtractionExpression(lhsSca, rhsSca)
          case _    => throw new VectorizationException("cannot deal with assignment operator \"" + assOp + "\" in " + stmt.prettyprint())
        }))
        SimplifyStrategy.doUntilDoneStandalone(srcWrap)
        SimplifyStrategy.doUntilDoneStandalone(lhsSca) // simplify lhsSca too, to ensure identical array accesses have the same AST structure
        // create rhs before lhs to ensure all loads are created
        val rhsVec = vectorizeExpr(srcWrap.expression, ctx.setLoad())
        val lhsVec = vectorizeExpr(lhsSca, ctx.setStore())
        // ---- special handling of loop-carried cse variables ----
        lhsSca match {
          case ArrayAccess(_ : iv.LoopCarriedCSBuffer, _, _) =>
            val initOpt : Option[SIMD_ConcShift] = ctx.toFinish_LCSE.get(ctx.getName(lhsSca)._1)
            if (initOpt.isDefined) {
              val concShiftRight : VariableAccess =
                rhsVec match {
                  case va : VariableAccess => Duplicate(va)
                  case _ => throw new VectorizationException("cannot vectorize code with lcse buffer and conventional CSE disabled")
                }
              initOpt.get.right = concShiftRight
            }
          case _ => // nothing to do
        }
        // --------------------------------------------------------
        ctx.addStmt(new AssignmentStatement(lhsVec, rhsVec, "="))
        if (ctx.storesTmp != null)
          ctx.addStmt(ctx.storesTmp)
        ctx.storesTmp = null

      case VariableDeclarationStatement(dataType, name, Some(init)) =>
        ctx.addStmt(new CommentStatement(stmt.prettyprint()))
        val initWrap = new ExpressionStatement(Duplicate(init))
        SimplifyStrategy.doUntilDoneStandalone(initWrap)
        val initVec = vectorizeExpr(initWrap.expression, ctx.setLoad())
        val (vecTmp : String, true) = ctx.getName(new VariableAccess(name, Some(dataType)))
        ctx.addStmt(new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, Some(initVec)))

      case ConditionStatement(cond, trueBody, falseBody) if (stmt.hasAnnotation(Vectorization.COND_VECTABLE)) =>
        if (stmt.hasAnnotation(Vectorization.COND_IGN_INCR))
          ctx.ignIncr = true
        ctx.pushScope()
        for (s <- trueBody)
          vectorizeStmt(s, ctx)
        val trueBodyVec = ctx.popScope()
        ctx.pushScope()
        for (s <- falseBody)
          vectorizeStmt(s, ctx)
        val falseBodyVec = ctx.popScope()
        ctx.ignIncr = false
        val njuCond = new ConditionStatement(Duplicate(cond), trueBodyVec, falseBodyVec)
        ctx.addStmt(njuCond)

      case _ => throw new VectorizationException("cannot deal with " + stmt.getClass() + "; " + stmt.prettyprint())
    }
  }

  private def vectorizeExpr(expr : Expression, ctx : LoopCtx) : Expression = {
    return expr match {
      // TODO: do not vectorize if base is not aligned?
      case ArrayAccess(base, index, alignedBase) =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp) {
          val ind : HashMap[Expression, Long] = SimplifyExpression.extractIntegralSum(index)
          val const : Option[Long] = ind.remove(SimplifyExpression.constName)
          var access1 : Boolean = true
          for ((expr, value) <- ind)
            expr match {
              case VariableAccess(name, Some(IntegerDatatype)) =>
                if (name == ctx.itName) {
                  if (value != 1L || ctx.incr != 1L)
                    throw new VectorizationException("no linear memory access;  loop increment: " + ctx.incr + "  index: " + index.prettyprint())
                  access1 = false
                }

              case DivisionExpression(
                VariableAccess(name, Some(IntegerDatatype)),
                IntegerConstant(divs)) =>
                if (name == ctx.itName) {
                  if (value != 1L || ctx.incr != divs)
                    throw new VectorizationException("no linear memory access;  loop increment: " + ctx.incr + "  index: " + index.prettyprint())
                  access1 = false
                }

              case DivisionExpression(
                AdditionExpression(ListBuffer(VariableAccess(name, Some(IntegerDatatype)), IntegerConstant(_))),
                IntegerConstant(divs)) =>
                if (name == ctx.itName) {
                  if (value != 1L || ctx.incr != divs)
                    throw new VectorizationException("no linear memory access;  loop increment: " + ctx.incr + "  index: " + index.prettyprint())
                  access1 = false
                }

              case DivisionExpression(
                AdditionExpression(ListBuffer(IntegerConstant(_), VariableAccess(name, Some(IntegerDatatype)))),
                IntegerConstant(divs)) =>
                if (name == ctx.itName) {
                  if (value != 1L || ctx.incr != divs)
                    throw new VectorizationException("no linear memory access;  loop increment: " + ctx.incr + "  index: " + index.prettyprint())
                  access1 = false
                }

              case _ =>
                if (containsVarAcc(expr, ctx.itName))
                  throw new VectorizationException("no linear memory access;  " + index.prettyprint())
            }
          if (ctx.ignIncr) // that means we basically only compute a scalar value
            access1 = true

          val vs = Platform.simd_vectorSize
          val aligned : Boolean = alignedBase && (const.getOrElse(0L) - ctx.getAlignedResidue()) % vs == 0
          base match {
            // ---- special handling of loop-carried cse variables ----
            case _ : iv.LoopCarriedCSBuffer if (access1) => //if(access1 && ctx.isStore() && !ctx.isLoad()) =>
              ctx.addStmtPreLoop(new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, SIMD_Scalar2VectorExpression(expr)), expr)
              ctx.addStmtPostLoop(new AssignmentStatement(expr, new SIMD_ExtractScalarExpression(new VariableAccess(vecTmp, SIMD_RealDatatype), vs - 1)))
              // ------------------------------------------------------
            case _ if (ctx.isLoad() && !ctx.isStore()) =>
              val init = Some(createLoadExpression(expr, base, ind, const.getOrElse(0L), access1, aligned, alignedBase, ctx))
              ctx.addStmt(new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, init))
            case _ if (!ctx.isLoad() && ctx.isStore()) =>
              ctx.addStmt(new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, None))
            case _ =>
              Logger.error("Only expected 'load XOR store', when vectorizing an ArrayAccess")
          }
          ctx.setAlignAndAccess1(vecTmp, aligned, access1)
        }
        val (aligned : Boolean, access1 : Boolean) = ctx.getAlignAndAccess1(vecTmp)
        if (ctx.isStore()) {
          // ---- special handling of loop-carried cse variables ----
          if (!base.isInstanceOf[iv.LoopCarriedCSBuffer] || !access1) { // if we have an access to a single LCS buffer, we must not do anything special here, just skip all sanity checks
            // ------------------------------------------------------
            if (access1)
              throw new VectorizationException("parallel store to a single memory location")
            if (!aligned && !alignedBase && Knowledge.simd_avoidUnaligned)
              throw new VectorizationException("cannot vectorize store: array is not aligned, but unaligned accesses should be avoided")
            if (ctx.storesTmp != null)
              Logger.debug("[vect] Error? More than one store in a single statement?!")
            ctx.storesTmp = new SIMD_StoreStatement(AddressofExpression(expr),
              new VariableAccess(vecTmp, SIMD_RealDatatype), aligned)
          }
        }
        // ---- special handling of loop-carried cse variables ----
        base match {
          case _ : iv.LoopCarriedCSBuffer if(access1 && ctx.isLoad() && !ctx.isStore() && !ctx.toFinish_LCSE.contains(vecTmp)) =>
            val init = new SIMD_ConcShift(new VariableAccess(vecTmp, SIMD_RealDatatype), null, Platform.simd_vectorSize - 1)
            ctx.toFinish_LCSE(vecTmp) = init
            ctx.addStmt(new AssignmentStatement(new VariableAccess(vecTmp, SIMD_RealDatatype), init, "="))
          case _ => // nothing to do
        }
        // --------------------------------------------------------
        new VariableAccess(vecTmp, SIMD_RealDatatype)

      case VariableAccess(name, dType) =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp) {
          val decl = new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp)
          if (ctx.isLoad())
            decl.expression = Some(new SIMD_Scalar2VectorExpression(new VariableAccess(name, dType)))
          if (name == ctx.itName) {
            if (ctx.isStore()) throw new VectorizationException("iteration variable is modified inside the loop body...")
            if (!ctx.ignIncr)
              decl.expression = Some(new SIMD_AdditionExpression(decl.expression.get, ctx.getIncrVector()))
            ctx.addStmt(decl)
          } else
            ctx.addStmtPreLoop(decl, expr)
        }
        new VariableAccess(vecTmp, SIMD_RealDatatype)

      case StringLiteral("omp_get_thread_num()") =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp) {
          val decl = new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, new SIMD_Scalar2VectorExpression(expr))
          ctx.addStmtPreLoop(decl, expr)
        }
        new VariableAccess(vecTmp, SIMD_RealDatatype)

      case FloatConstant(value) =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          ctx.addStmtPreLoop(new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, new SIMD_Scalar2VectorExpression(FloatConstant(value))), expr)
        new VariableAccess(vecTmp, SIMD_RealDatatype)

      case IntegerConstant(value) => // TODO: ensure type safety
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          ctx.addStmtPreLoop(new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, new SIMD_Scalar2VectorExpression(FloatConstant(value))), expr)
        new VariableAccess(vecTmp, SIMD_RealDatatype)

      case NegativeExpression(nExpr) =>
        SIMD_NegateExpression(vectorizeExpr(nExpr, ctx))

      case AdditionExpression(sums) =>
        if (sums.isEmpty)
          Logger.error("empty sum not allowed")
        val (muls, other) = sums.partition(_.isInstanceOf[MultiplicationExpression])
        val mulsIt = muls.iterator
        val vecSumds = new Queue[Expression]()
        vecSumds.enqueue(other.view.map { x => vectorizeExpr(x, ctx) } : _*)
        if (vecSumds.isEmpty)
          vecSumds += vectorizeExpr(mulsIt.next(), ctx)
        while (mulsIt.hasNext) {
          val simdMul = vectorizeExpr(mulsIt.next(), ctx).asInstanceOf[SIMD_MultiplicationExpression]
          vecSumds.enqueue(SIMD_MultiplyAddExpression(simdMul.left, simdMul.right, vecSumds.dequeue()))
        }
        while (vecSumds.length > 1)
          vecSumds.enqueue(SIMD_AdditionExpression(vecSumds.dequeue(), vecSumds.dequeue()))
        vecSumds.dequeue()

      case SubtractionExpression(left, right) =>
        SIMD_SubtractionExpression(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))

      case MultiplicationExpression(facs) =>
        if (facs.isEmpty)
          Logger.error("empty product not allowed")
        val exprs = new Queue[Expression]()
        exprs.enqueue(facs.view.map { x => vectorizeExpr(x, ctx) } : _*)
        while (exprs.length > 1)
          exprs.enqueue(SIMD_MultiplicationExpression(exprs.dequeue(), exprs.dequeue()))
        exprs.dequeue()

      case DivisionExpression(left, right) =>
        SIMD_DivisionExpression(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))

      case MinimumExpression(args) =>
        if (args.isEmpty)
          Logger.error("empty minimum not allowed")
        val exprs = new Queue[Expression]()
        exprs.enqueue(args.view.map { x => vectorizeExpr(x, ctx) } : _*)
        while (exprs.length > 1)
          exprs.enqueue(SIMD_MinimumExpression(exprs.dequeue(), exprs.dequeue()))
        exprs.dequeue()

      case MaximumExpression(args) =>
        if (args.isEmpty)
          Logger.error("empty minimum not allowed")
        val exprs = new Queue[Expression]()
        exprs.enqueue(args.view.map { x => vectorizeExpr(x, ctx) } : _*)
        while (exprs.length > 1)
          exprs.enqueue(SIMD_MaximumExpression(exprs.dequeue(), exprs.dequeue()))
        exprs.dequeue()

      case FunctionCallExpression(func, args) if (SIMD_MathFunctions.isAllowed(func)) =>
        FunctionCallExpression(SIMD_MathFunctions.addUsage(func), args.map { arg => vectorizeExpr(arg, ctx) })

      case PowerExpression(base, exp) if (SIMD_MathFunctions.isAllowed("pow")) =>
        FunctionCallExpression(SIMD_MathFunctions.addUsage("pow"), ListBuffer(vectorizeExpr(base, ctx), vectorizeExpr(exp, ctx)))

      case mAcc : MemberAccess =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          ctx.addStmtPreLoop(new VariableDeclarationStatement(SIMD_RealDatatype, vecTmp, SIMD_Scalar2VectorExpression(mAcc)), expr)
        new VariableAccess(vecTmp, SIMD_RealDatatype)

      case _ =>
        throw new VectorizationException("cannot deal with " + expr.getClass() + "; " + expr.prettyprint())
    }
  }

  private def createLoadExpression(oldExpr : Expression, base : Expression,
      index : HashMap[Expression, Long], indexConst : Long,
      access1 : Boolean, aligned : Boolean, alignedBase : Boolean, ctx : LoopCtx) : Expression = {

    if (access1)
      return SIMD_Scalar2VectorExpression(oldExpr)
    else if (aligned || !Knowledge.simd_avoidUnaligned)
      return SIMD_LoadExpression(AddressofExpression(oldExpr), aligned)
    else if (!alignedBase)
      throw new VectorizationException("cannot vectorize load: array is not aligned, but unaligned accesses should be avoided")
    else { // avoid unaligned load
      val vs : Long = Platform.simd_vectorSize
      val lowerConst : Long = indexConst - ((indexConst - ctx.getAlignedResidue()) % vs + vs) % vs
      index(SimplifyExpression.constName) = lowerConst
      val lowerExpr = vectorizeExpr(ArrayAccess(base, SimplifyExpression.recreateExprFromIntSum(index), true), ctx).asInstanceOf[VariableAccess]
      index(SimplifyExpression.constName) = lowerConst + vs
      val upperExpr = vectorizeExpr(ArrayAccess(base, SimplifyExpression.recreateExprFromIntSum(index), true), ctx).asInstanceOf[VariableAccess]
      return SIMD_ConcShift(lowerExpr, upperExpr, (indexConst - lowerConst).toInt)
    }
  }
}
