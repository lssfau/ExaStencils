package exastencils.optimization

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import exastencils.core.Duplicate
import exastencils.core.Logger
import exastencils.core.StateManager
import exastencils.core.collectors.Collector
import exastencils.datastructures.CustomStrategy
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.convFromListBuffer
import exastencils.datastructures.Transformation.convFromNode
import exastencils.datastructures.ir.AdditionExpression
import exastencils.datastructures.ir.ArrayAccess
import exastencils.datastructures.ir.AssignmentStatement
import exastencils.datastructures.ir.BitwiseAndExpression
import exastencils.datastructures.ir.CommentStatement
import exastencils.datastructures.ir.DivisionExpression
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.FloatConstant
import exastencils.datastructures.ir.ForLoopStatement
import exastencils.datastructures.ir.IntegerConstant
import exastencils.datastructures.ir.IntegerDatatype
import exastencils.datastructures.ir.LowerEqualExpression
import exastencils.datastructures.ir.LowerExpression
import exastencils.datastructures.ir.MultiplicationExpression
import exastencils.datastructures.ir.NullStatement
import exastencils.datastructures.ir.SIMD_AdditionExpression
import exastencils.datastructures.ir.SIMD_DivisionExpression
import exastencils.datastructures.ir.SIMD_FloatConstantExpression
import exastencils.datastructures.ir.SIMD_Load1Expression
import exastencils.datastructures.ir.SIMD_LoadExpression
import exastencils.datastructures.ir.SIMD_MultiplicationExpression
import exastencils.datastructures.ir.SIMD_MultiplyAddExpression
import exastencils.datastructures.ir.SIMD_MultiplySubExpression
import exastencils.datastructures.ir.SIMD_RealDatatype
import exastencils.datastructures.ir.SIMD_StoreStatement
import exastencils.datastructures.ir.SIMD_SubtractionExpression
import exastencils.datastructures.ir.Statement
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.datastructures.ir.UnaryExpression
import exastencils.datastructures.ir.UnaryOperators
import exastencils.datastructures.ir.VariableAccess
import exastencils.datastructures.ir.VariableDeclarationStatement
import exastencils.util.SimplifyExpression
import exastencils.datastructures.ir.VariableDeclarationStatement
import exastencils.datastructures.ir.SIMD_RealDatatype
import exastencils.datastructures.ir.SIMD_Scalar2VectorExpression
import exastencils.datastructures.ir.RealDatatype
import exastencils.datastructures.ir.SIMD_RealDatatype
import exastencils.datastructures.ir.StatementBlock
import exastencils.datastructures.ir.FunctionCallExpression

object Vectorization extends CustomStrategy("Vectorization") {

  final val TMP_VS : Int = 4 // TODO: replace with actual hardware info

  private[optimization] final val VECT_ANNOT = "Vectorize"

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    val annotate = new AnnotateLoops()
    StateManager.register(annotate)
    this.execute(new Transformation("find loops", PartialFunction.empty))
    StateManager.unregister(annotate)

    this.execute(new Transformation("optimize", VectorizeAnnotated))

    this.commit()
  }
}

private final class AnnotateLoops extends Collector {

  private var innerLoop : Boolean = false

  def enter(n : Node) : Unit = {
    innerLoop = innerLoop || n.isInstanceOf[ForLoopStatement with OptimizationHint]
  }

  def leave(n : Node) : Unit = {
    if (!innerLoop)
      return

    n match {
      case loop : ForLoopStatement with OptimizationHint =>
        innerLoop = false
        loop.annotate(Vectorization.VECT_ANNOT)
      case _ =>
    }
  }

  def reset() : Unit = {
    innerLoop = false
  }
}

private final case class VectorizationException(msg : String) extends Exception(msg)

private final object VectorizeAnnotated extends PartialFunction[Node, Transformation.Output[_]] {

  def isDefinedAt(node : Node) : Boolean = {
    return node.hasAnnotation(Vectorization.VECT_ANNOT)
  }

  def apply(node : Node) : Transformation.Output[_] = {

    return try {
      vectorizeLoop(node.asInstanceOf[ForLoopStatement])
    } catch {
      case VectorizationException(msg) =>
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
        AssignmentStatement(assignVar, assignExpr, assignOp),
        body, None) //
        =>

        assignVar match {
          case VariableAccess(v, _) if (v == itName) =>
          case StringConstant(v) if (v == itName)    =>
          case _ =>
            throw new VectorizationException("initialized variable is not incremented in inc loop header")
        }

        var uBoundExcl : Expression = null
        condition match {

          case LowerExpression(VariableAccess(boundsName, _), upperBoundExcl) if (itName == boundsName) =>
            uBoundExcl = upperBoundExcl
          case LowerExpression(StringConstant(boundsName), upperBoundExcl) if (itName == boundsName) =>
            uBoundExcl = upperBoundExcl

          case LowerEqualExpression(VariableAccess(boundsName, _), upperBoundIncl) if (itName == boundsName) =>
            uBoundExcl = AdditionExpression(upperBoundIncl, IntegerConstant(1))
          case LowerEqualExpression(StringConstant(boundsName), upperBoundIncl) if (itName == boundsName) =>
            uBoundExcl = AdditionExpression(upperBoundIncl, IntegerConstant(1))

          case _ => throw new VectorizationException("no upper bound")
        }

        assignOp match {

          case "+=" =>
            assignExpr match {
              case IntegerConstant(1) => vectorizeLoop(itName, lBound, uBoundExcl, body)
              case _                  => throw new VectorizationException("loop increment 1 required for vectorization")
            }

          case "=" =>
            assignExpr match {
              case AdditionExpression(IntegerConstant(1), VariableAccess(v, _)) if v == itName =>
                vectorizeLoop(itName, lBound, uBoundExcl, body)
              case AdditionExpression(VariableAccess(v, _), IntegerConstant(1)) if v == itName =>
                vectorizeLoop(itName, lBound, uBoundExcl, body)
              case AdditionExpression(IntegerConstant(1), StringConstant(v)) if v == itName =>
                vectorizeLoop(itName, lBound, uBoundExcl, body)
              case AdditionExpression(StringConstant(v), IntegerConstant(1)) if v == itName =>
                vectorizeLoop(itName, lBound, uBoundExcl, body)
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
    body : ListBuffer[Statement]) : Transformation.Output[_] = {

    val ctx = new LoopCtx(itName)
    for (stmt <- body)
      vectorizeStmt(stmt, ctx)

    def itVar = VariableAccess(itName, Some(IntegerDatatype()))
    val res = new ListBuffer[Node]()
    res += VariableDeclarationStatement(IntegerDatatype(), itName, Some(begin))
    res += ForLoopStatement(NullStatement(),
      LowerExpression(itVar, BitwiseAndExpression(AdditionExpression(begin, IntegerConstant(Vectorization.TMP_VS - 1)),
        UnaryExpression(UnaryOperators.BitwiseNegation, IntegerConstant(Vectorization.TMP_VS - 1)))),
      AssignmentStatement(itVar, IntegerConstant(1), "+="),
      Duplicate(body))
    res ++= ctx.preLoopStmts
    res += ForLoopStatement(NullStatement(),
      LowerExpression(itVar, SubtractionExpression(endExcl, IntegerConstant(Vectorization.TMP_VS))),
      AssignmentStatement(itVar, IntegerConstant(Vectorization.TMP_VS), "+="),
      ctx.vectStmts)
    res += ForLoopStatement(NullStatement(),
      LowerExpression(itVar, endExcl),
      AssignmentStatement(itVar, IntegerConstant(1), "+="),
      body) // old AST will be replaced completely, so we can reuse the body once here (we cloned above)

    return res
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

      case StatementBlock(stmts : ListBuffer[Statement]) =>
        for (s <- stmts)
          vectorizeStmt(s, ctx)

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
              case VariableAccess(name, _) if (name == ctx.itName) =>
                if (value == 1L) access1 = false
                else throw new VectorizationException("no linear memory access")

              case _ =>
                val containsLoopVar = new DefaultStrategy("")
                containsLoopVar += new Transformation("", {
                  case node @ VariableAccess(name, _) if (name == ctx.itName) =>
                    throw new VectorizationException("no linear memory access")
                  case node => node
                })
                containsLoopVar.applyStandalone(expr)
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
          ctx.preLoopStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp,
            Some(SIMD_FloatConstantExpression(value)))
        VariableAccess(vecTmp, Some(SIMD_RealDatatype()))

      case IntegerConstant(value) => // TODO: ensure type safety
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          ctx.preLoopStmts += VariableDeclarationStatement(SIMD_RealDatatype(), vecTmp,
            Some(SIMD_FloatConstantExpression(value)))
        VariableAccess(vecTmp, Some(SIMD_RealDatatype()))

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
