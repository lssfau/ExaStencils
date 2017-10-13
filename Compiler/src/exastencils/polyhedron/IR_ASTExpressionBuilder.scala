package exastencils.polyhedron

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_NeighborIsValid

object IR_ASTExpressionBuilder {

  /** Convert an isl.AstExpr to an IR_Expression. */
  def processIslExpr(expr : isl.AstExpr) : IR_Expression = {

    expr.getType match { // TODO: check if ExprId contains only variable identifier
      case isl.AstExprType.ExprId    =>
        val id : String = expr.getId.getName
        Duplicate(ScopNameMapping.id2expr(id)).getOrElse(IR_StringLiteral(id))
      case isl.AstExprType.ExprInt   => IR_IntegerConstant(expr.getVal.toString.toLong)
      case isl.AstExprType.ExprOp    => processIslExprOp(expr)
      case isl.AstExprType.ExprError => throw PolyASTBuilderException("ExprError found...")
    }
  }

  /** Process an isl.AstExpr of type isl.AstExprType.ExprOp. Caller must ensure only this type of node is passed! */
  private def processIslExprOp(expr : isl.AstExpr) : IR_Expression = {

    val args : Array[IR_Expression] = processArgs(expr)
    val n : Int = args.length

    expr.getOpType match {
      case isl.AstOpType.OpEq if n == 2 && args(0).isInstanceOf[IR_IV_NeighborIsValid] =>
        args(1) match {
          case IR_IntegerConstant(1) => args(0)
          case IR_IntegerConstant(0) => IR_Negation(args(0))
        }

      case isl.AstOpType.OpAndThen if n == 2 => IR_AndAnd(args(0), args(1))
      case isl.AstOpType.OpAnd if n == 2     => IR_AndAnd(args(0), args(1))
      case isl.AstOpType.OpOrElse if n == 2  => IR_OrOr(args(0), args(1))
      case isl.AstOpType.OpOr if n == 2      => IR_OrOr(args(0), args(1))
      case isl.AstOpType.OpMinus if n == 1   => IR_Negative(args(0))
      case isl.AstOpType.OpAdd if n == 2     => IR_Addition(args(0), args(1))
      case isl.AstOpType.OpSub if n == 2     => IR_Subtraction(args(0), args(1))
      case isl.AstOpType.OpMul if n == 2     => IR_Multiplication(args(0), args(1))
      case isl.AstOpType.OpDiv if n == 2     => IR_Division(args(0), args(1))
      case isl.AstOpType.OpFdivQ if n == 2   => IR_FunctionCall("floord", args(0), args(1)) // TODO: ensure integer division
      case isl.AstOpType.OpPdivQ if n == 2   => IR_Division(args(0), args(1)) // TODO: ensure integer division
      case isl.AstOpType.OpPdivR if n == 2   => IR_Modulo(args(0), args(1))
      case isl.AstOpType.OpZdivR if n == 2   => IR_Modulo(args(0), args(1)) // isl doc: Equal to zero iff the remainder on integer division is zero.
      case isl.AstOpType.OpCond if n == 3    => IR_TernaryCondition(args(0), args(1), args(2))
      case isl.AstOpType.OpEq if n == 2      => IR_EqEq(args(0), args(1))
      case isl.AstOpType.OpLe if n == 2      => IR_LowerEqual(args(0), args(1))
      case isl.AstOpType.OpLt if n == 2      => IR_Lower(args(0), args(1))
      case isl.AstOpType.OpGe if n == 2      => IR_GreaterEqual(args(0), args(1))
      case isl.AstOpType.OpGt if n == 2      => IR_Greater(args(0), args(1))
      case isl.AstOpType.OpMax if n >= 2     => IR_Maximum(args : _*)
      case isl.AstOpType.OpMin if n >= 2     => IR_Minimum(args : _*)
      case isl.AstOpType.OpSelect if n == 3  => IR_TernaryCondition(args(0), args(1), args(2))

      case isl.AstOpType.OpCall if n >= 1 =>
        val fArgs = ListBuffer[IR_Expression](args : _*)
        fArgs.remove(0)
        IR_FunctionCall(args(0).asInstanceOf[IR_StringLiteral].value, fArgs)

      case err =>
        throw PolyASTBuilderException("expression not (yet) available:  " + err + "  with " + args.length + " arguments:  " + expr)
    }
  }

  /** Convert arguments of an isl.AstExpr to an array of IR_Expression. */
  def processArgs(expr : isl.AstExpr) : Array[IR_Expression] = {

    val nArgs : Int = expr.getOpNArg
    val args = new Array[IR_Expression](nArgs)
    for (i <- 0 until nArgs)
      args(i) = processIslExpr(expr.getOpArg(i))

    args
  }
}
