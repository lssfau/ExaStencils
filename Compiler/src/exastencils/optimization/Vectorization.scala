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
    val storesTmp = new ListBuffer[Statement]()

    val preLoopStmts = new ListBuffer[Statement]()

    private final val temporaryMapping = new HashMap[Expression, String]
    private final val temporaryProperties = new HashMap[String, (Boolean, Boolean)]
    private var isStore_ : Boolean = false
    private var varID : Int = -1

    def getName(expr : Expression) : (String, Boolean) = {
      var nju : Boolean = false
      val name : String = temporaryMapping.getOrElseUpdate(expr, {
        nju = true
        varID += 1
        "vec" + varID
      })
      return (name, nju)
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

    for (stmt <- body)
      vectorizeStmt(stmt, ctx)

    def itVar = VariableAccess(itName, Some(IntegerDatatype()))
    val res = new ListBuffer[Statement]()

    res += new VariableDeclarationStatement(IntegerDatatype(), itName, Some(begin))
    val upperPreName = "_upperPre"
    def upperPre = VariableAccess(upperPreName, Some(IntegerDatatype()))
    res += new VariableDeclarationStatement(IntegerDatatype(), upperPreName, Some(Duplicate(begin)))
    val upperAligned = {
      def alM1 = IntegerConstant(Knowledge.simd_vectorSize - 1)
      def bNeg(expr : Expression) = UnaryExpression(UnaryOperators.BitwiseNegation, expr)
      def incrC = IntegerConstant(incr)
      if (incr == 1L)
        BitwiseAndExpression(upperPre + alM1, bNeg(alM1))
      else
        (BitwiseAndExpression((upperPre / incrC) + alM1, bNeg(alM1)) * incrC) + (upperPre Mod incrC)
    }
    res += new AssignmentStatement(upperPre, MinimumExpression(ListBuffer(upperAligned, Duplicate(endExcl))), "=")

    res += new ForLoopStatement(NullStatement,
      LowerExpression(itVar, upperPre),
      AssignmentStatement(itVar, IntegerConstant(incr), "+="),
      Duplicate(body))

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
    oldLoop.begin = AssignmentStatement(itVar, upperPre)
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
        ctx.vectStmts ++= ctx.storesTmp
        ctx.storesTmp.clear()

      case _ => throw new VectorizationException("cannot deal with " + stmt.getClass() + "; " + stmt.prettyprint())
    }
  }

  private def vectorizeExpr(expr : Expression, ctx : LoopCtx) : Expression = {
    return expr match {
      case ArrayAccess(base, index) =>
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
                Logger.setLevel(1)
                try {
                  // "wrap" expr to ensure expr itself is affected by trafo, too
                  containsLoopVar.applyStandalone(FreeStatement(expr))
                } finally {
                  Logger.setLevel(oldLvl)
                }
            }

          // TODO: ensure grid alignment first
          val aligned : Boolean = (const.getOrElse(0L) % Knowledge.simd_vectorSize) == 0
          var init : Option[Expression] =
            if (ctx.isLoad() && !ctx.isStore())
              Some(createLoadExpression(expr, base, ind, const.getOrElse(0L), access1, aligned, ctx))
            else if (!ctx.isLoad() && ctx.isStore())
              None
            else
              Logger.error("Only expected 'load XOR store', when vectorizing an ArrayAccess")
          ctx.vectStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp, init)
          ctx.setAlignAndAccess1(vecTmp, aligned, access1)
        }
        if (ctx.isStore()) {
          val (aligned : Boolean, access1 : Boolean) = ctx.getAlignAndAccess1(vecTmp)
          if (access1) throw new VectorizationException("parallel store to a single memory location")
          ctx.storesTmp += SIMD_StoreStatement(UnaryExpression(UnaryOperators.AddressOf, expr),
            VariableAccess(vecTmp, Some(SIMD_RealDatatype())), aligned)
        }
        VariableAccess(vecTmp, Some(SIMD_RealDatatype()))

      case VariableAccess(name, dType) =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          if (name == ctx.itName) {
            if (ctx.isStore()) throw new VectorizationException("iteration variable is modified inside the loop body...")
            ctx.vectStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp,
              Some(SIMD_Scalar2VectorExpression(name, SIMD_RealDatatype(), true)))

          } else
            ctx.preLoopStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp,
              Some(SIMD_Scalar2VectorExpression(name, SIMD_RealDatatype(), false)))
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
    access1 : Boolean, aligned : Boolean, ctx : LoopCtx) : Expression = {

    if (access1)
      return SIMD_Load1Expression(UnaryExpression(UnaryOperators.AddressOf, oldExpr))
    else if (aligned || !Knowledge.simd_avoidUnaligned)
      return SIMD_LoadExpression(UnaryExpression(UnaryOperators.AddressOf, oldExpr), aligned)
    else { // avoid unaligned load
      val lowerConst : Long = indexConst & ~(Knowledge.simd_vectorSize - 1)
      index(SimplifyExpression.constName) = lowerConst
      val lowerExpr = vectorizeExpr(ArrayAccess(base, SimplifyExpression.recreateExprFromIntSum(index)), ctx).asInstanceOf[VariableAccess]
      index(SimplifyExpression.constName) = lowerConst + Knowledge.simd_vectorSize
      val upperExpr = vectorizeExpr(ArrayAccess(base, SimplifyExpression.recreateExprFromIntSum(index)), ctx).asInstanceOf[VariableAccess]
      return SIMD_ConcShift(lowerExpr, upperExpr, (indexConst - lowerConst).toInt)
    }
  }
}
