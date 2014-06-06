package exastencils.polyhedron

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

import exastencils.core.Logger
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.convFromNode
import exastencils.datastructures.ir.AdditionExpression
import exastencils.datastructures.ir.AndAndExpression
import exastencils.datastructures.ir.AssignmentStatement
import exastencils.datastructures.ir.ConditionStatement
import exastencils.datastructures.ir.DivisionExpression
import exastencils.datastructures.ir.EqEqExpression
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.ForLoopStatement
import exastencils.datastructures.ir.FunctionCallExpression
import exastencils.datastructures.ir.GreaterEqualExpression
import exastencils.datastructures.ir.GreaterExpression
import exastencils.datastructures.ir.IntegerConstant
import exastencils.datastructures.ir.IntegerDatatype
import exastencils.datastructures.ir.LowerEqualExpression
import exastencils.datastructures.ir.LowerExpression
import exastencils.datastructures.ir.ModuloExpression
import exastencils.datastructures.ir.MultiplicationExpression
import exastencils.datastructures.ir.OrOrExpression
import exastencils.datastructures.ir.Scope
import exastencils.datastructures.ir.Statement
import exastencils.datastructures.ir.StatementBlock
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.datastructures.ir.TernaryConditionExpression
import exastencils.datastructures.ir.UnaryExpression
import exastencils.datastructures.ir.UnaryOperators
import exastencils.datastructures.ir.VariableAccess
import exastencils.datastructures.ir.VariableDeclarationStatement
import exastencils.knowledge.Knowledge
import exastencils.knowledge.dimToString
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.primitives.LoopOverDimensions
import isl.Conversions.convertLambdaToXCallback1

class ASTBuilderTransformation(replaceCallback : (String, Expression, Node) => Unit)
  extends Transformation("insert optimized loop AST", new ASTBuilderFunction(replaceCallback))

private final class ASTBuilderFunction(replaceCallback : (String, Expression, Node) => Unit)
    extends PartialFunction[Node, Transformation.Output[_]] {

  private final val ZERO_VAL : isl.Val = isl.Val.zero()
  private final val ONE_VAL : isl.Val = isl.Val.one()

  private var oldStmts : HashMap[String, Statement] = null
  private var seqDims : HashSet[String] = null
  private var parallelize : Boolean = false

  def isDefinedAt(node : Node) : Boolean = node match {
    case loop : LoopOverDimensions with PolyhedronAccessable =>
      loop.hasAnnotation(PolyOpt.SCOP_ANNOT)
    case _ => false
  }

  def apply(node : Node) : Transformation.Output[_] = {

    val scop : Scop = node.removeAnnotation(PolyOpt.SCOP_ANNOT).get.value.asInstanceOf[Scop]

    //    Logger.debug("create AST for model:")
    //    Logger.debug("    Domain:   " + scop.domain)
    //    Logger.debug("    Schedule: " + scop.schedule)

    val islBuild : isl.AstBuild = isl.AstBuild.fromContext(scop.domain.params())
    val islNode : isl.AstNode = islBuild.astFromSchedule(scop.schedule.intersectDomain(scop.domain))

    oldStmts = scop.stmts
    if (scop.root.asInstanceOf[LoopOverDimensions].parallelizationIsReasonable) {
      parallelize = true
      seqDims = new HashSet[String]()

      scop.deps.foreachMap({ dep : isl.Map =>
        val directions = dep.deltas()
        val universe : isl.Set = isl.BasicSet.universe(directions.getSpace())
        val dim : Int = universe.dim(isl.DimType.Set)
        var i = 0
        while (i < dim) {

          var seq = universe
          var j = 0
          while (j < i) {
            seq = seq.fixVal(isl.DimType.Set, j, ZERO_VAL)
            j += 1
          }
          seq = seq.lowerBoundVal(isl.DimType.Set, i, ONE_VAL)

          if (!seq.intersect(directions).isEmpty())
            seqDims.add(seq.getDimName(isl.DimType.Set, i))

          i += 1
        }
      })
    }

    try {
      processIslNode(islNode)
    } catch {
      case PolyASTBuilderException(msg) =>
        Logger.debug("[poly] cannot create AST from model:  " + msg)
        node
    }
  }

  private def processIslNode(node : isl.AstNode) : Statement = {

    return node.getType() match {

      case isl.AstNodeType.NodeFor =>
        if (node.forIsDegenerate() != 0) {

          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType() == isl.AstExprType.ExprId, "isl for node iterator is not an ExprId")
          val it : VariableAccess = VariableAccess(islIt.getId().getName(), Some(IntegerDatatype()))
          val decl : Statement = new VariableDeclarationStatement(it, Some(processIslExpr(node.forGetInit())))

          Scope(ListBuffer[Statement](decl, processIslNode(node.forGetBody())))

        } else {

          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType() == isl.AstExprType.ExprId, "isl for node iterator is not an ExprId")
          val itStr : String = islIt.getId().getName()
          val parNow : Boolean = parallelize && !seqDims.contains(itStr)
          parallelize &= !parNow // if code must be parallelized, then now (parNow) xor later (parallelize)
          val it : VariableAccess = VariableAccess(itStr, Some(IntegerDatatype()))
          val init : Statement = new VariableDeclarationStatement(it, Some(processIslExpr(node.forGetInit())))
          val cond : Expression = processIslExpr(node.forGetCond())
          val incr : Statement = AssignmentStatement(it, AdditionExpression(it, processIslExpr(node.forGetInc())))

          val body : Statement = processIslNode(node.forGetBody())
          parallelize |= parNow // restore overall parallelization level
          if (parNow)
            body match {
              case StatementBlock(raw) => new ForLoopStatement(init, cond, incr, raw) with OMP_PotentiallyParallel
              case _                   => new ForLoopStatement(init, cond, incr, body) with OMP_PotentiallyParallel
            }
          else
            body match {
              case StatementBlock(raw) => new ForLoopStatement(init, cond, incr, raw)
              case _                   => new ForLoopStatement(init, cond, incr, body)
            }
        }

      case isl.AstNodeType.NodeIf =>
        val cond : Expression = processIslExpr(node.ifGetCond())
        val thenBranch : Statement = processIslNode(node.ifGetThen())
        if (node.ifHasElse() != 0) {
          val els : Statement = processIslNode(node.ifGetElse())
          new ConditionStatement(cond, thenBranch, els)
        } else
          new ConditionStatement(cond, thenBranch)

      case isl.AstNodeType.NodeBlock =>
        val stmts = new ListBuffer[Statement]
        node.blockGetChildren().foreach({ stmt : isl.AstNode => stmts += processIslNode(stmt); () })
        if (stmts.length == 1)
          stmts(0)
        else
          new StatementBlock(stmts)

      case isl.AstNodeType.NodeUser =>
        val expr : isl.AstExpr = node.userGetExpr()
        assume(expr.getOpType() == isl.AstOpType.OpCall, "user node is no OpCall?!")
        val args : Array[Expression] = processArgs(expr)
        val name : String = args(0).asInstanceOf[StringConstant].value
        val stmt : Statement = oldStmts(name)
        var d : Int = 1
        val dims : Int = Knowledge.dimensionality
        do {
          replaceCallback(dimToString(dims - d), args(d), stmt)
          d += 1
        } while (d < args.length)
        stmt

      case isl.AstNodeType.NodeError => throw new PolyASTBuilderException("NodeError found...")
    }
  }

  private def processIslExpr(expr : isl.AstExpr) : Expression = {

    return expr.getType() match { // TODO: check if ExprId contains only variable identifier
      case isl.AstExprType.ExprId =>
        val id : String = expr.getId().getName()
        ScopNameMapping.id2expr(id).getOrElse(StringConstant(id))
      case isl.AstExprType.ExprInt   => IntegerConstant(expr.getVal().toString().toLong)
      case isl.AstExprType.ExprOp    => processIslExprOp(expr)
      case isl.AstExprType.ExprError => throw new PolyASTBuilderException("ExprError found...")
    }
  }

  /** Process an isl.AstExpr of type isl.AstExprType.ExprOp. Caller must ensure only this type of node is passed! */
  private def processIslExprOp(expr : isl.AstExpr) : Expression = {

    val args : Array[Expression] = processArgs(expr)
    val n : Int = args.length

    return expr.getOpType() match {
      case isl.AstOpType.OpAndThen if n == 2 => AndAndExpression(args(0), args(1))
      case isl.AstOpType.OpOrElse if n == 2  => OrOrExpression(args(0), args(1))
      case isl.AstOpType.OpMax if n == 2     => FunctionCallExpression(StringConstant("std::max"), ListBuffer(args : _*))
      case isl.AstOpType.OpMin if n == 2     => FunctionCallExpression(StringConstant("std::min"), ListBuffer(args : _*))
      case isl.AstOpType.OpMinus if n == 1   => UnaryExpression(UnaryOperators.Negative, args(0))
      case isl.AstOpType.OpAdd if n == 2     => AdditionExpression(args(0), args(1))
      case isl.AstOpType.OpSub if n == 2     => SubtractionExpression(args(0), args(1))
      case isl.AstOpType.OpMul if n == 2     => MultiplicationExpression(args(0), args(1))
      case isl.AstOpType.OpDiv if n == 2     => DivisionExpression(args(0), args(1))
      case isl.AstOpType.OpFdivQ if n == 2   => DivisionExpression(args(0), args(1)) // TODO: ensure integer division; round to -inf for negative
      case isl.AstOpType.OpPdivQ if n == 2   => DivisionExpression(args(0), args(1)) // TODO: ensure integer division
      case isl.AstOpType.OpPdivR if n == 2   => ModuloExpression(args(0), args(1))
      case isl.AstOpType.OpCond if n == 3    => TernaryConditionExpression(args(0), args(1), args(2))
      case isl.AstOpType.OpEq if n == 2      => EqEqExpression(args(0), args(1))
      case isl.AstOpType.OpLe if n == 2      => LowerEqualExpression(args(0), args(1))
      case isl.AstOpType.OpLt if n == 2      => LowerExpression(args(0), args(1))
      case isl.AstOpType.OpGe if n == 2      => GreaterEqualExpression(args(0), args(1))
      case isl.AstOpType.OpGt if n == 2      => GreaterExpression(args(0), args(1))

      case isl.AstOpType.OpCall if n >= 1 =>
        val fArgs = ListBuffer[Expression](args : _*)
        fArgs.remove(0)
        FunctionCallExpression(args(0), fArgs)

      case err =>
        throw new PolyASTBuilderException("expression not (yet) available:  " + err + "  with " + args.length + " arguments:  " + expr)
    }
  }

  private def processArgs(expr : isl.AstExpr) : Array[Expression] = {

    val nArgs : Int = expr.getOpNArg()
    val args = new Array[Expression](nArgs)
    var i : Int = 0;
    while (i < nArgs) {
      args(i) = processIslExpr(expr.getOpArg(i))
      i += 1
    }

    return args
  }
}

case class PolyASTBuilderException(msg : String = null) extends Exception(msg) {}
