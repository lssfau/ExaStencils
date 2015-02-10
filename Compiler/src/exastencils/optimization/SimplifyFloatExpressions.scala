package exastencils.optimization

import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.logger._
import exastencils.util._

object SimplifyFloatExpressions extends DefaultStrategy("Simplify floating expressions") {

  private final val DEBUG : Boolean = false

  this += new Transformation("optimize", {
    case d @ VariableDeclarationStatement(RealDatatype(), _, Some(expr)) =>
      d.expression = Some(simplify(expr))
      d

    case a @ AssignmentStatement(VariableAccess(_, Some(RealDatatype())), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(VariableAccess(_, Some(PointerDatatype(RealDatatype()))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(VariableAccess(_, Some(ConstPointerDatatype(RealDatatype()))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(VariableAccess(_, Some(ArrayDatatype(RealDatatype(), _))), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(fd : iv.FieldData, _, _), src, op) //
    if (fd.field.dataType.resolveUnderlyingDatatype == RealDatatype()) =>
      a.src = simplify(src)
      a
  })

  private final def simplify(expr : Expression) : Expression = {
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
