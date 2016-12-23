package exastencils.optimization

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger._
import exastencils.optimization.ir._

object SimplifyFloatExpressions extends DefaultStrategy("Simplify floating expressions") {

  private final val DEBUG : Boolean = false

  this += new Transformation("optimize", {
    case d @ IR_VariableDeclaration(IR_RealDatatype, _, Some(expr)) =>
      d.initialValue = Some(simplify(expr))
      d

    case a @ IR_Assignment(IR_VariableAccess(_, IR_RealDatatype), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, IR_PointerDatatype(IR_RealDatatype)), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, IR_CUDAConstPointerDatatype(IR_RealDatatype)), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, IR_ConstPointerDatatype(IR_RealDatatype)), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, IR_ArrayDatatype(IR_RealDatatype, _)), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(fd : IR_IV_FieldData, _, _), src, op) //
      if fd.field.resolveBaseDatatype == IR_RealDatatype =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(fa : IR_MultiDimFieldAccess, src, op) =>
      a.src = simplify(src)
      a
  })

  private final def simplify(expr : IR_Expression) : IR_Expression = {
    try {
      IR_SimplifyExpression.simplifyFloatingExpr(expr)
    } catch {
      case x : EvaluationException =>
        if (DEBUG)
          println("[simplify]  cannot simplify float expression: " + x.msg) // print directly, logger may be silenced by any surrounding strategy
        expr
    }
  }
}
