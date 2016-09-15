package exastencils.optimization

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.logger._
import exastencils.util._

object SimplifyFloatExpressions extends DefaultStrategy("Simplify floating expressions") {

  private final val DEBUG : Boolean = false

  this += new Transformation("optimize", {
    case d @ IR_VariableDeclaration(IR_RealDatatype, _, Some(expr)) =>
      d.initialValue = Some(simplify(expr))
      d

    case a @ IR_Assignment(IR_VariableAccess(_, Some(IR_RealDatatype)), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, Some(IR_PointerDatatype(IR_RealDatatype))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, Some(IR_CUDAConstPointerDatatype(IR_RealDatatype))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, Some(IR_ConstPointerDatatype(IR_RealDatatype))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, Some(IR_ArrayDatatype(IR_RealDatatype, _))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(fd : iv.FieldData, _, _), src, op) //
      if (fd.field.resolveBaseDatatype == IR_RealDatatype) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(fa : IR_MultiDimFieldAccess, src, op) =>
      a.src = simplify(src)
      a
  })

  private final def simplify(expr : IR_Expression) : IR_Expression = {
    try {
      return SimplifyExpression.simplifyFloatingExpr(expr)
    } catch {
      case x : EvaluationException =>
        if (DEBUG)
          Logger.debug("[simplify]  cannot simplify float expression: " + x.msg)
        return expr
    }
  }
}
