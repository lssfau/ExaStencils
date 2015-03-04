package exastencils.optimization

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.omp._
import exastencils.util._

object Vectorization extends DefaultStrategy("Vectorization") {

  final val VECT_ANNOT = "VECT"

  this += new Transformation("optimize", VectorizeInnermost)
}

private final case class VectorizationException(msg : String) extends Exception(msg)

private final object VectorizeInnermost extends PartialFunction[Node, Transformation.OutputType] {

  private final val DEBUG : Boolean = false

  override def isDefinedAt(node : Node) : Boolean = {
    return node match {
      case loop : ForLoopStatement with OptimizationHint =>
        loop.isInnermost && loop.isParallel
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
      case ForLoopStatement(VariableDeclarationStatement(IntegerDatatype(), itName, Some(lBound)), condExpr, incrExpr, body, reduction) =>

        val uBoundExcl : Expression =
          condExpr match {
            case LowerExpression(VariableAccess(bName, Some(IntegerDatatype())), upperBoundExcl) if (itName == bName) =>
              upperBoundExcl
            case LowerEqualExpression(VariableAccess(bName, Some(IntegerDatatype())), upperBoundIncl) if (itName == bName) =>
              AdditionExpression(upperBoundIncl, IntegerConstant(1))
            case _ => throw new VectorizationException("no upper bound")
          }

        val incr : Long =
          incrExpr match {
            case ExpressionStatement(PreIncrementExpression(VariableAccess(n, Some(IntegerDatatype())))) if (itName == n)  => 1L
            case ExpressionStatement(PostIncrementExpression(VariableAccess(n, Some(IntegerDatatype())))) if (itName == n) => 1L
            case AssignmentStatement(VariableAccess(n, Some(IntegerDatatype())),
              IntegerConstant(i),
              "+=") if (itName == n) => i
            case AssignmentStatement(VariableAccess(n1, Some(IntegerDatatype())),
              AdditionExpression(IntegerConstant(i), VariableAccess(n2, Some(IntegerDatatype()))),
              "=") if (itName == n1 && itName == n2) => i
            case AssignmentStatement(VariableAccess(n1, Some(IntegerDatatype())),
              AdditionExpression(VariableAccess(n2, Some(IntegerDatatype())), IntegerConstant(i)),
              "=") if (itName == n1 && itName == n2) => i
            case _ => throw new VectorizationException("loop increment must be constant or cannot be extracted:  " + incrExpr)
          }

        vectorizeLoop(loop, itName, lBound, uBoundExcl, incr, body, reduction)

      case _ => throw new VectorizationException("cannot analyze loop (yet)")
    }
  }

  private final class LoopCtx(val itName : String, val incr : Long) {

    val vectStmts = new ListBuffer[Statement]()
    var storesTmp : Statement = null

    val preLoopStmts = new ListBuffer[Statement]()

    private final val temporaryMapping = new HashMap[Expression, String]
    private final val temporaryProperties = new HashMap[String, (Boolean, Boolean)]
    private var isStore_ : Boolean = false
    private var varID : Int = -1
    private var incrVectDeclared : Boolean = false
    private var alignedResidue : Long = -1

    def getName(expr : Expression) : (String, Boolean) = {
      var nju : Boolean = false
      val name : String = temporaryMapping.getOrElseUpdate(expr, {
        nju = true
        varID += 1
        "vec" + varID
      })
      return (name, nju)
    }

    def getIncrVector() : VariableAccess = {
      val name : String = "veci"
      if (!incrVectDeclared) {
        SIMD_IncrementVectorDeclaration(name) +=: preLoopStmts
        incrVectDeclared = true
      }
      return VariableAccess(name, Some(SIMD_RealDatatype()))
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

  private def vectorizeLoop(oldLoop : ForLoopStatement, itName : String, begin : Expression, endExcl : Expression,
    incr : Long, body : ListBuffer[Statement], reduction : Option[Reduction]) : Statement = {

    val ctx = new LoopCtx(itName, incr)
    var postLoopStmt : Statement = null
    if (reduction.isDefined) {
      val target = reduction.get.target
      val operator = reduction.get.op

      val (vecTmp, _) : (String, Boolean) = ctx.getName(target)
      val identityElem : Expression =
        operator match {
          case "+"   => SIMD_FloatConstant(0.0)
          case "*"   => SIMD_FloatConstant(1.0)
          case "min" => SIMD_FloatConstant(Double.MaxValue)
          case "max" => SIMD_FloatConstant(Double.MinValue)
          case _     => throw new VectorizationException("unknown reduction operator:  " + operator)
        }
      ctx.preLoopStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp, Some(identityElem))

      val vecTmpAcc = VariableAccess(vecTmp, Some(SIMD_RealDatatype()))
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
    if (Knowledge.data_alignFieldPointers) {
      val vs = Knowledge.simd_vectorSize
      for (stmt <- body)
        stmt match {
          case AssignmentStatement(acc @ ArrayAccess(_, index, true), _, _) =>
            val annot = acc.getAnnotation(AddressPrecalculation.ORIG_IND_ANNOT)
            val ind : Expression = if (annot.isDefined) annot.get.value.asInstanceOf[Expression] else index
            val const : Long = SimplifyExpression.extractIntegralSum(ind).get(SimplifyExpression.constName).getOrElse(0L)
            val residue : Long = (const % vs + vs) % vs
            ctx.setAlignedResidue(residue)
            alignmentExpr = ind
          case _ =>
        }

      val indexExprs = new ListBuffer[HashMap[Expression, Long]]()
      val collectIndexExprs = new DefaultStrategy("Collect all array index expressions...")
      collectIndexExprs += new Transformation("seaching...", {
        case acc @ ArrayAccess(_, index, true) =>
          val annot = acc.removeAnnotation(AddressPrecalculation.ORIG_IND_ANNOT)
          indexExprs += SimplifyExpression.extractIntegralSum(if (annot.isDefined) annot.get.value.asInstanceOf[Expression] else index)
          acc
      })
      val oldLvl = Logger.getLevel
      Logger.setLevel(Logger.WARNING)
      collectIndexExprs.applyStandalone(Scope(body))
      Logger.setLevel(oldLvl)

      // no store available, so align as max loads as possible
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

    def itVar = VariableAccess(itName, Some(IntegerDatatype()))
    val res = new ListBuffer[Statement]()

    val startName = "_start"
    def start = VariableAccess(startName, Some(IntegerDatatype()))
    res += new VariableDeclarationStatement(IntegerDatatype(), startName, Some(begin))
    res += new VariableDeclarationStatement(IntegerDatatype(), itName, Some(begin))

    if (Knowledge.data_alignFieldPointers) { // no need to ensure alignment of iteration variable if data is not aligned
      res += new ForLoopStatement(NullStatement,
        AndAndExpression(NeqNeqExpression(Duplicate(alignmentExpr), IntegerConstant(ctx.getAlignedResidue())),
          LowerExpression(itVar, Duplicate(endExcl))),
        AssignmentStatement(itVar, IntegerConstant(incr), "+="),
        Duplicate(body))

      res += new AssignmentStatement(start, itVar, "=")
    }

    res ++= ctx.preLoopStmts

    val upperName = "_upper"
    def upper = VariableAccess(upperName, Some(IntegerDatatype()))
    res += new VariableDeclarationStatement(IntegerDatatype(), upperName, Some(endExcl))

    // no rollback after this point allowed!
    // reuse oldLoop to preserve all traits
    // set first value (again) to allow omp parallelization
    val annot : Option[Annotation] = oldLoop.removeAnnotation(Unrolling.NO_REM_ANNOT)
    val oldOffset : Long = if (annot.isDefined) annot.get.value.asInstanceOf[Long] else 0L
    val endOffset : Long = incr * Knowledge.simd_vectorSize - 1 - oldOffset
    oldLoop.begin = AssignmentStatement(itVar, start)
    oldLoop.end = LowerExpression(itVar, SubtractionExpression(upper, IntegerConstant(endOffset)))
    oldLoop.inc = AssignmentStatement(itVar, IntegerConstant(incr * Knowledge.simd_vectorSize), "+=")
    oldLoop.body = ctx.vectStmts
    oldLoop.annotate(Unrolling.NO_REM_ANNOT, endOffset)
    oldLoop.annotate(Vectorization.VECT_ANNOT)
    if (oldLoop.isInstanceOf[OMP_PotentiallyParallel]) // allow omp parallelization
      oldLoop.asInstanceOf[OMP_PotentiallyParallel].addOMPStatements += "lastprivate(" + itName + ')'
    res += oldLoop

    if (postLoopStmt != null)
      res += postLoopStmt

    res += new ForLoopStatement(NullStatement,
      LowerExpression(itVar, upper),
      AssignmentStatement(itVar, IntegerConstant(incr), "+="),
      body) // old AST will be replaced completely, so we can reuse the body once here (we cloned above)

    return new Scope(res)
  }

  private def vectorizeStmt(stmt : Statement, ctx : LoopCtx) : Unit = {
    stmt match {
      case CommentStatement(str) =>
        ctx.vectStmts += CommentStatement(str) // new instance

      case AssignmentStatement(lhsSca, rhsSca, assOp) =>
        ctx.vectStmts += new CommentStatement(stmt.prettyprint())
        val source = assOp match {
          case "="  => rhsSca
          case "+=" => AdditionExpression(Duplicate(lhsSca), rhsSca)
          case "-=" => SubtractionExpression(Duplicate(lhsSca), rhsSca)
          case _    => throw new VectorizationException("cannot deal with assignment operator \"" + assOp + "\" in " + stmt.prettyprint())
        }
        // create rhs before lhs to ensure all loads are created
        val rhsVec = vectorizeExpr(source, ctx.setLoad())
        val lhsVec = vectorizeExpr(lhsSca, ctx.setStore())
        ctx.vectStmts += AssignmentStatement(lhsVec, rhsVec, "=")
        ctx.vectStmts += ctx.storesTmp
        ctx.storesTmp = null

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
              case VariableAccess(name, Some(IntegerDatatype())) =>
                if (name == ctx.itName) {
                  if (value != 1L || ctx.incr != 1L) throw new VectorizationException("no linear memory access;  " + expr.prettyprint())
                  access1 = false
                }

              case DivisionExpression(
                VariableAccess(name, Some(IntegerDatatype())),
                IntegerConstant(divs)) =>
                if (name == ctx.itName) {
                  if (value != 1L || ctx.incr != divs) throw new VectorizationException("no linear memory access;  " + expr.prettyprint())
                  access1 = false
                }

              case DivisionExpression(
                AdditionExpression(VariableAccess(name, Some(IntegerDatatype())), IntegerConstant(_)),
                IntegerConstant(divs)) =>
                if (name == ctx.itName) {
                  if (value != 1L || ctx.incr != divs) throw new VectorizationException("no linear memory access;  " + expr.prettyprint())
                  access1 = false
                }

              case DivisionExpression(
                AdditionExpression(IntegerConstant(_), VariableAccess(name, Some(IntegerDatatype()))),
                IntegerConstant(divs)) =>
                if (name == ctx.itName) {
                  if (value != 1L || ctx.incr != divs) throw new VectorizationException("no linear memory access;  " + expr.prettyprint())
                  access1 = false
                }

              case _ =>
                val containsLoopVar = new DefaultStrategy("Ensure linear memory access")
                containsLoopVar += new Transformation("seaching...", {
                  case node @ VariableAccess(name, _) if (name == ctx.itName) =>
                    throw new VectorizationException("no linear memory access;  " + expr.prettyprint())
                })
                val oldLvl = Logger.getLevel
                Logger.setLevel(Logger.WARNING)
                try {
                  // "wrap" expr to ensure expr itself is affected by trafo, too
                  containsLoopVar.applyStandalone(FreeStatement(expr))
                } finally {
                  Logger.setLevel(oldLvl)
                }
            }

          val vs = Knowledge.simd_vectorSize
          val aligned : Boolean = alignedBase && (const.getOrElse(0L) - ctx.getAlignedResidue()) % vs == 0
          var init : Option[Expression] =
            if (ctx.isLoad() && !ctx.isStore())
              Some(createLoadExpression(expr, base, ind, const.getOrElse(0L), access1, aligned, alignedBase, ctx))
            else if (!ctx.isLoad() && ctx.isStore())
              None
            else
              Logger.error("Only expected 'load XOR store', when vectorizing an ArrayAccess")
          ctx.vectStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp, init)
          ctx.setAlignAndAccess1(vecTmp, aligned, access1)
        }
        if (ctx.isStore()) {
          val (aligned : Boolean, access1 : Boolean) = ctx.getAlignAndAccess1(vecTmp)
          if (access1)
            throw new VectorizationException("parallel store to a single memory location")
          if (!aligned && !alignedBase && Knowledge.simd_avoidUnaligned)
            throw new VectorizationException("cannot vectorize store: array is not aligned, but unaligned accesses should be avoided")
          if (ctx.storesTmp != null)
            Logger.debug("[vect] Error? More than one store in a single statement?!")
          ctx.storesTmp = SIMD_StoreStatement(UnaryExpression(UnaryOperators.AddressOf, expr),
            VariableAccess(vecTmp, Some(SIMD_RealDatatype())), aligned)
        }
        VariableAccess(vecTmp, Some(SIMD_RealDatatype()))

      case VariableAccess(name, dType) =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          if (name == ctx.itName) {
            if (ctx.isStore()) throw new VectorizationException("iteration variable is modified inside the loop body...")
            ctx.vectStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp,
              Some(SIMD_AdditionExpression(SIMD_Scalar2VectorExpression(VariableAccess(name, dType)), ctx.getIncrVector())))

          } else
            ctx.preLoopStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp,
              Some(SIMD_Scalar2VectorExpression(VariableAccess(name, dType))))
        VariableAccess(vecTmp, Some(SIMD_RealDatatype()))

      case FloatConstant(value) =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          ctx.preLoopStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp, Some(SIMD_FloatConstant(value)))
        VariableAccess(vecTmp, Some(SIMD_RealDatatype()))

      case IntegerConstant(value) => // TODO: ensure type safety
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          ctx.preLoopStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp, Some(SIMD_FloatConstant(value)))
        VariableAccess(vecTmp, Some(SIMD_RealDatatype()))

      case UnaryExpression(UnaryOperators.Negative, expr) =>
        SIMD_NegateExpresseion(vectorizeExpr(expr, ctx))

      case AdditionExpression(MultiplicationExpression(factor1, factor2), summand) =>
        SIMD_MultiplyAddExpression(vectorizeExpr(factor1, ctx), vectorizeExpr(factor2, ctx), vectorizeExpr(summand, ctx))

      case AdditionExpression(summand, MultiplicationExpression(factor1, factor2)) =>
        SIMD_MultiplyAddExpression(vectorizeExpr(factor1, ctx), vectorizeExpr(factor2, ctx), vectorizeExpr(summand, ctx))

      case AdditionExpression(left, right) =>
        SIMD_AdditionExpression(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))

      case SubtractionExpression(MultiplicationExpression(factor1, factor2), summand) =>
        SIMD_MultiplySubExpression(vectorizeExpr(factor1, ctx), vectorizeExpr(factor2, ctx), vectorizeExpr(summand, ctx))

      case SubtractionExpression(left, right) =>
        SIMD_SubtractionExpression(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))

      case MultiplicationExpression(left, right) =>
        SIMD_MultiplicationExpression(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))

      case DivisionExpression(left, right) =>
        SIMD_DivisionExpression(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))

      case _ =>
        throw new VectorizationException("cannot deal with " + expr.getClass() + "; " + expr.prettyprint())
    }
  }

  private def createLoadExpression(oldExpr : Expression, base : Expression,
    index : HashMap[Expression, Long], indexConst : Long,
    access1 : Boolean, aligned : Boolean, alignedBase : Boolean, ctx : LoopCtx) : Expression = {

    if (access1)
      return SIMD_Load1Expression(UnaryExpression(UnaryOperators.AddressOf, oldExpr))
    else if (aligned || !Knowledge.simd_avoidUnaligned)
      return SIMD_LoadExpression(UnaryExpression(UnaryOperators.AddressOf, oldExpr), aligned)
    else if (!alignedBase)
      throw new VectorizationException("cannot vectorize load: array is not aligned, but unaligned accesses should be avoided")
    else { // avoid unaligned load
      val vs : Long = Knowledge.simd_vectorSize
      val lowerConst : Long = indexConst - ((indexConst - ctx.getAlignedResidue()) % vs + vs) % vs
      index(SimplifyExpression.constName) = lowerConst
      val lowerExpr = vectorizeExpr(ArrayAccess(base, SimplifyExpression.recreateExprFromIntSum(index), true), ctx).asInstanceOf[VariableAccess]
      index(SimplifyExpression.constName) = lowerConst + vs
      val upperExpr = vectorizeExpr(ArrayAccess(base, SimplifyExpression.recreateExprFromIntSum(index), true), ctx).asInstanceOf[VariableAccess]
      return SIMD_ConcShift(lowerExpr, upperExpr, (indexConst - lowerConst).toInt)
    }
  }
}
