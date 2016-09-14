package exastencils.optimization

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.logger._
import exastencils.util._

object SimplifyFloatExpressions extends DefaultStrategy("Simplify floating expressions") {

  private final val DEBUG : Boolean = false

  this += new Transformation("optimize", {
    case d @ VariableDeclarationStatement(IR_RealDatatype, _, Some(expr)) =>
      d.expression = Some(simplify(expr))
      d

    case a @ AssignmentStatement(IR_VariableAccess(_, Some(IR_RealDatatype)), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(IR_VariableAccess(_, Some(IR_PointerDatatype(IR_RealDatatype))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(IR_VariableAccess(_, Some(IR_CUDAConstPointerDatatype(IR_RealDatatype))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(IR_VariableAccess(_, Some(IR_ConstPointerDatatype(IR_RealDatatype))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(IR_VariableAccess(_, Some(IR_ArrayDatatype(IR_RealDatatype, _))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(fd : iv.FieldData, _, _), src, op) //
      if (fd.field.resolveBaseDatatype == IR_RealDatatype) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(fa : FieldAccessLike, src, op) =>
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
