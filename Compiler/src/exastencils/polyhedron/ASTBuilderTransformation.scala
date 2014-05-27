package exastencils.polyhedron

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Annotation
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
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.datastructures.ir.TernaryConditionExpression
import exastencils.datastructures.ir.UnaryExpression
import exastencils.datastructures.ir.UnaryOperators
import exastencils.datastructures.ir.VariableAccess
import exastencils.datastructures.ir.VariableDeclarationStatement
import exastencils.knowledge.Knowledge
import exastencils.knowledge.dimToString
import exastencils.primitives.LoopOverDimensions
import isl.Conversions.convertLambdaToXCallback1

class ASTBuilderTransformation(replaceCallback : (String, Expression, Node) => Unit)
  extends Transformation("insert optimized loop AST", new ASTBuilderFunction(replaceCallback))

private final class ASTBuilderFunction(replaceCallback : (String, Expression, Node) => Unit)
    extends PartialFunction[Node, Transformation.Output[_]] {

  def isDefinedAt(node : Node) : Boolean = node match {
    case loop : LoopOverDimensions with PolyhedronAccessable =>
      loop.hasAnnotation(PolyOpt.SCOP_ANNOT)
    case _ => false
  }

  def apply(node : Node) : Transformation.Output[_] = {

    val annotation : Annotation = node.getAnnotation(PolyOpt.SCOP_ANNOT).get
    node.remove(annotation)
    val scop : SCoP = annotation.value.asInstanceOf[SCoP]

    val islBuild : isl.AstBuild = isl.AstBuild.fromContext(scop.domain.params())
    val islNode : isl.AstNode = islBuild.astFromSchedule(scop.schedule.intersectDomain(scop.domain))

    return processIslNode(islNode, scop.stmts)
  }

  private def processIslNode(node : isl.AstNode, oldStmts : HashMap[String, Statement]) : Statement = {

    return node.getType() match {

      case isl.AstNodeType.NodeFor =>
        if (node.forIsDegenerate() != 0) {

          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType() == isl.AstExprType.ExprId, "isl for node iterator is not an ExprId")
          val it : VariableAccess = new VariableAccess(islIt.getId().getName(), Some(new IntegerDatatype()))
          val decl : Statement = new VariableDeclarationStatement(it, Some(processIslExpr(node.forGetInit())))

          val stmts : ListBuffer[Statement] = new ListBuffer[Statement]()
          stmts += decl
          stmts += processIslNode(node.forGetBody(), oldStmts)
          new Scope(stmts)

        } else {

          val islIt : isl.AstExpr = node.forGetIterator()
          assume(islIt.getType() == isl.AstExprType.ExprId, "isl for node iterator is not an ExprId")
          val it : VariableAccess = new VariableAccess(islIt.getId().getName(), Some(new IntegerDatatype()))
          val init : Statement = new VariableDeclarationStatement(it, Some(processIslExpr(node.forGetInit())))
          val cond : Expression = processIslExpr(node.forGetCond())
          val incr : Statement = new AssignmentStatement(it, new AdditionExpression(it, processIslExpr(node.forGetInc())))

          val body : Statement = processIslNode(node.forGetBody(), oldStmts)
          body match {
            case StatementBlock(raw) => new ForLoopStatement(init, cond, incr, raw)
            case _                   => new ForLoopStatement(init, cond, incr, body)
          }
        }

      case isl.AstNodeType.NodeIf =>
        val cond : Expression = processIslExpr(node.ifGetCond())
        val thenBranch : Statement = processIslNode(node.ifGetThen(), oldStmts)
        if (node.ifHasElse() != 0) {
          val els : Statement = processIslNode(node.ifGetElse(), oldStmts)
          new ConditionStatement(cond, thenBranch, els)
        } else
          new ConditionStatement(cond, thenBranch)

      case isl.AstNodeType.NodeBlock =>
        val stmts = new ListBuffer[Statement]
        node.blockGetChildren().foreach({ stmt : isl.AstNode => stmts += processIslNode(stmt, oldStmts); () })
        if (stmts.length == 1)
          stmts(0)
        else
          new StatementBlock(stmts)

      case isl.AstNodeType.NodeUser =>
        val expr : isl.AstExpr = node.userGetExpr()
        assume(expr.getOpType() == isl.AstOpType.OpCall, "user node is no OpCall?!")
        val args : Array[Expression] = processArgs(expr)
        val name : String = args(0).asInstanceOf[VariableAccess].name
        val stmt : Statement = oldStmts(name)
        var d : Int = 1
        val dims : Int = Knowledge.dimensionality
        do { // TODO: build declarations for old loop iterators
          replaceCallback(dimToString(dims - d), args(d), stmt)
          d += 1
        } while (d < args.length)
        stmt

      case isl.AstNodeType.NodeError => throw new PolyASTBuilderException("NodeError found...")
    }
  }

  private def processIslExpr(expr : isl.AstExpr) : Expression = {

    return expr.getType() match { // TODO: check if ExprId contains only variable identifier
      case isl.AstExprType.ExprId    => new VariableAccess(expr.getId().getName())
      case isl.AstExprType.ExprInt   => new IntegerConstant(expr.getVal().toString().toLong)
      case isl.AstExprType.ExprOp    => processIslExprOp(expr)
      case isl.AstExprType.ExprError => throw new PolyASTBuilderException("ExprError found...")
    }
  }

  /** Process an isl.AstExpr of type isl.AstExprType.ExprOp. Caller must ensure only this type of node is passed! */
  private def processIslExprOp(expr : isl.AstExpr) : Expression = {

    val args : Array[Expression] = processArgs(expr)

    return expr.getOpType() match {
      case isl.AstOpType.OpAnd                           => throw new PolyASTBuilderException("eager logical 'and' not available in AST")
      case isl.AstOpType.OpAndThen if (args.length == 2) => new AndAndExpression(args(0), args(1))
      case isl.AstOpType.OpOr                            => throw new PolyASTBuilderException("eager logical 'or' not available in AST")
      case isl.AstOpType.OpOrElse if (args.length == 2)  => new OrOrExpression(args(0), args(1))
      case isl.AstOpType.OpMax                           => throw new PolyASTBuilderException("maximum not yet available")
      case isl.AstOpType.OpMin                           => throw new PolyASTBuilderException("minimum not yet available")
      case isl.AstOpType.OpMinus if (args.length == 1)   => new UnaryExpression(UnaryOperators.Negative, args(0))
      case isl.AstOpType.OpAdd if (args.length == 2)     => new AdditionExpression(args(0), args(1))
      case isl.AstOpType.OpSub if (args.length == 2)     => new SubtractionExpression(args(0), args(1))
      case isl.AstOpType.OpMul if (args.length == 2)     => new MultiplicationExpression(args(0), args(1))
      case isl.AstOpType.OpDiv if (args.length == 2)     => new DivisionExpression(args(0), args(1))
      case isl.AstOpType.OpFdivQ                         => throw new PolyASTBuilderException("division (round to -inf) not yet available")
      case isl.AstOpType.OpPdivQ if (args.length == 2)   => new DivisionExpression(args(0), args(1)) // TODO: ensure integer division
      case isl.AstOpType.OpPdivR if (args.length == 2)   => new ModuloExpression(args(0), args(1))
      case isl.AstOpType.OpCond if (args.length == 3)    => new TernaryConditionExpression(args(0), args(1), args(2))
      case isl.AstOpType.OpSelect                        => throw new PolyASTBuilderException("eager ternary operator not available in AST")
      case isl.AstOpType.OpEq if (args.length == 2)      => new EqEqExpression(args(0), args(1))
      case isl.AstOpType.OpLe if (args.length == 2)      => new LowerEqualExpression(args(0), args(1))
      case isl.AstOpType.OpLt if (args.length == 2)      => new LowerExpression(args(0), args(1))
      case isl.AstOpType.OpGe if (args.length == 2)      => new GreaterEqualExpression(args(0), args(1))
      case isl.AstOpType.OpGt if (args.length == 2)      => new GreaterExpression(args(0), args(1))

      case isl.AstOpType.OpCall if (args.length >= 1) =>
        val fArgs : ListBuffer[Expression] = new ListBuffer[Expression]()
        (fArgs ++= args).remove(0)
        new FunctionCallExpression(args(0), fArgs)

      case isl.AstOpType.OpAccess => throw new PolyASTBuilderException("array access not yet implemented")
      case isl.AstOpType.OpMember => throw new PolyASTBuilderException("member access not yet implemented")

      case isl.AstOpType.OpError  => throw new PolyASTBuilderException("OpError found...")
      case err =>
        throw new PolyASTBuilderException("unexpected expression type: " + err + "; #arguments: " + args.length)
    }
  }

  private def processArgs(expr : isl.AstExpr) : Array[Expression] = {

    val nArgs : Int = expr.getOpNArg()
    val args : Array[Expression] = new Array[Expression](nArgs)
    var i : Int = 0;
    while (i < nArgs) {
      args(i) = processIslExpr(expr.getOpArg(i))
      i += 1
    }

    return args
  }
}

case class PolyASTBuilderException(msg : String = null) extends Exception(msg) {}
