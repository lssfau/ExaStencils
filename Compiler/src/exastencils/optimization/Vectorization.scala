package exastencils.optimization

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate
import exastencils.core.Logger
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge.Knowledge
import exastencils.util.SimplifyExpression

object Vectorization extends DefaultStrategy("Vectorization") {
  this += new Transformation("optimize", VectorizeInnermost)
}

private final case class VectorizationException(msg : String) extends Exception(msg)

private final object VectorizeInnermost extends PartialFunction[Node, Transformation.Output[_]] {

  private final val DEBUG : Boolean = false

  def isDefinedAt(node : Node) : Boolean = {
    return node match {
      case loop : ForLoopStatement with OptimizationHint =>
        loop.isInnermost && loop.isParallel
      case _ =>
        false
    }
  }

  def apply(node : Node) : Transformation.Output[_] = {

    return try {
      vectorizeLoop(node.asInstanceOf[ForLoopStatement])
    } catch {
      case VectorizationException(msg) =>
        if (DEBUG)
          Logger.debug("[vect]  unable to vectorize loop: " + msg)
        node
    }
  }

  private def vectorizeLoop(loop : ForLoopStatement) : Transformation.Output[_] = {

    // excessive testing if loop header allows vectorization
    return loop match {
      case ForLoopStatement(
        VariableDeclarationStatement(IntegerDatatype(), itName, Some(lBound)),
        condition,
        AssignmentStatement(VariableAccess(itName2, Some(IntegerDatatype())), assignExpr, assignOp),
        body, reduction) //
        if (itName == itName2) =>

        val isInScope : Boolean = loop.hasAnnotation(InScope.ANNOT)

        val uBoundExcl : Expression =
          condition match {

            case LowerExpression(VariableAccess(boundsName, Some(IntegerDatatype())),
              upperBoundExcl) if (itName == boundsName) =>
              upperBoundExcl

            case LowerEqualExpression(VariableAccess(boundsName, Some(IntegerDatatype())),
              upperBoundIncl) if (itName == boundsName) =>
              AdditionExpression(upperBoundIncl, IntegerConstant(1))

            case _ => throw new VectorizationException("no upper bound")
          }

        assignOp match {

          case "+=" =>
            assignExpr match {
              case IntegerConstant(1) => vectorizeLoop(itName, lBound, uBoundExcl, body, reduction, isInScope)
              case _                  => throw new VectorizationException("loop increment 1 required for vectorization")
            }

          case "=" =>
            assignExpr match {
              case AdditionExpression(IntegerConstant(1), VariableAccess(v, Some(IntegerDatatype()))) if v == itName =>
                vectorizeLoop(itName, lBound, uBoundExcl, body, reduction, isInScope)
              case AdditionExpression(VariableAccess(v, Some(IntegerDatatype())), IntegerConstant(1)) if v == itName =>
                vectorizeLoop(itName, lBound, uBoundExcl, body, reduction, isInScope)
              case _ => throw new VectorizationException("loop increment 1 required for vectorization")
            }

          case _ => throw new VectorizationException("cannot deal with assignment operator \"" + assignOp + '"')
        }

      case _ => throw new VectorizationException("cannot analyze loop (yet)")
    }
  }

  private final class LoopCtx(val itName : String) {

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

  private def vectorizeLoop(itName : String, begin : Expression, endExcl : Expression,
    body : ListBuffer[Statement], reduction : Option[Reduction], isInScope : Boolean) : Transformation.Output[_] = {

    val ctx = new LoopCtx(itName)
    var postLoopStmt : Statement = null
    if (reduction.isDefined) {
      val target = reduction.get.target
      val operator = reduction.get.op

      val (vecTmp, _) : (String, Boolean) = ctx.getName(target)
      val identityElem : Expression =
        operator match {
          case BinaryOperators.Addition       => SIMD_FloatConstant(0.0)
          case BinaryOperators.Subtraction    => SIMD_FloatConstant(0.0)
          case BinaryOperators.Multiplication => SIMD_FloatConstant(1.0)
          case _ =>
            throw new VectorizationException("unknown reduction operator:  " + operator)
        }
      ctx.preLoopStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp, Some(identityElem))

      val vecTmpAcc = VariableAccess(vecTmp, Some(SIMD_RealDatatype()))
      postLoopStmt =
        operator match {
          case BinaryOperators.Addition | BinaryOperators.Subtraction =>
            SIMD_HorizontalAddStatement(target, vecTmpAcc, "+=")
          case BinaryOperators.Multiplication =>
            SIMD_HorizontalMulStatement(target, vecTmpAcc, "*=")
        }
    }

    for (stmt <- body)
      vectorizeStmt(stmt, ctx)

    def itVar = VariableAccess(itName, Some(IntegerDatatype()))
    val res = new ListBuffer[Statement]()

    res += new VariableDeclarationStatement(IntegerDatatype(), itName, Some(begin))

    res += new ForLoopStatement(NullStatement,
      LowerExpression(itVar, BitwiseAndExpression(AdditionExpression(begin, IntegerConstant(Knowledge.simd_vectorSize - 1)),
        UnaryExpression(UnaryOperators.BitwiseNegation, IntegerConstant(Knowledge.simd_vectorSize - 1)))),
      AssignmentStatement(itVar, IntegerConstant(1), "+="),
      Duplicate(body))

    res ++= ctx.preLoopStmts

    val vectLoop = new ForLoopStatement(NullStatement,
      LowerExpression(itVar, SubtractionExpression(endExcl, IntegerConstant(Knowledge.simd_vectorSize - 1))),
      AssignmentStatement(itVar, IntegerConstant(Knowledge.simd_vectorSize), "+="),
      ctx.vectStmts) with OptimizationHint
    vectLoop.isInnermost = true
    vectLoop.isParallel = true
    vectLoop.annotate(Unrolling.NO_REM_ANNOT, Knowledge.simd_vectorSize - 1)
    vectLoop.annotate(InScope.ANNOT)
    res += vectLoop

    if (postLoopStmt != null)
      res += postLoopStmt

    res += new ForLoopStatement(NullStatement,
      LowerExpression(itVar, endExcl),
      AssignmentStatement(itVar, IntegerConstant(1), "+="),
      body) // old AST will be replaced completely, so we can reuse the body once here (we cloned above)

    if (isInScope)
      return res
    else
      return new Scope(res)
  }

  private def vectorizeStmt(stmt : Statement, ctx : LoopCtx) : Unit = {

    if (stmt.isInstanceOf[CommentStatement]) {
      ctx.vectStmts += stmt
      return // nothing else to do for a simple comment
    }

    ctx.vectStmts += new CommentStatement(stmt.cpp())
    stmt match {
      case AssignmentStatement(lhsSca, rhsSca, assOp) =>
        val source = assOp match {
          case "="  => rhsSca
          case "+=" => AdditionExpression(lhsSca, rhsSca)
          case "-=" => SubtractionExpression(lhsSca, rhsSca)
          case _    => throw new VectorizationException("cannot deal with assignment operator \"" + assOp + "\" in " + stmt.cpp())
        }
        // create rhs before lhs to ensure all loads are created
        val rhsVec = vectorizeExpr(source, ctx.setLoad())
        val lhsVec = vectorizeExpr(lhsSca, ctx.setStore())
        ctx.vectStmts += AssignmentStatement(lhsVec, rhsVec, "=")

      case _ => throw new VectorizationException("cannot deal with " + stmt.getClass() + "; " + stmt.cpp())
    }

    ctx.vectStmts ++= ctx.storesTmp
    ctx.storesTmp.clear()
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
              case VariableAccess(name, Some(IntegerDatatype())) if (name == ctx.itName) =>
                if (value == 1L) access1 = false
                else throw new VectorizationException("no linear memory access")

              case _ =>
                val containsLoopVar = new DefaultStrategy("Ensure linear memory access")
                containsLoopVar += new Transformation("seaching...", {
                  case node @ VariableAccess(name, _) if (name == ctx.itName) =>
                    throw new VectorizationException("no linear memory access")
                })
                val oldLvl = Logger.getLevel
                Logger.setLevel(1)
                try {
                  containsLoopVar.applyStandalone(expr)
                } finally {
                  Logger.setLevel(oldLvl)
                }
            }

          // TODO: ensure grid alignment first
          val aligned : Boolean = false // (const.getOrElse(0L) % Vectorization.TMP_VS) == 0
          var init : Option[Expression] =
            if (ctx.isLoad())
              if (access1) Some(SIMD_Load1Expression(UnaryExpression(UnaryOperators.AddressOf, expr)))
              else Some(SIMD_LoadExpression(UnaryExpression(UnaryOperators.AddressOf, expr), aligned))
            else None
          ctx.vectStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp, init)
          ctx.setAlignAndAccess1(vecTmp, aligned, access1)
        }
        if (ctx.isStore()) {
          val (aligned : Boolean, access1 : Boolean) = ctx.getAlignAndAccess1(vecTmp)
          if (access1)
            throw new VectorizationException("parallel store to a single memory location")
          else
            ctx.storesTmp += SIMD_StoreStatement(UnaryExpression(UnaryOperators.AddressOf, expr),
              VariableAccess(vecTmp, Some(SIMD_RealDatatype())), aligned)
        }
        VariableAccess(vecTmp, Some(SIMD_RealDatatype()))

      case VariableAccess(name, dType) =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          if (name == ctx.itName) {
            if (ctx.isStore())
              throw new VectorizationException("iteration variable is modified inside the loop body...")
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
        throw new VectorizationException("cannot deal with " + expr.getClass() + "; " + expr.cpp())
    }
  }
}
