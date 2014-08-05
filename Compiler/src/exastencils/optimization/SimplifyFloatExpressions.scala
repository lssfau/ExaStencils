package exastencils.optimization

import exastencils.core.Logger
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.convFromNode
import exastencils.datastructures.ir.ArrayAccess
import exastencils.datastructures.ir.ArrayDatatype
import exastencils.datastructures.ir.AssignmentStatement
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.PointerDatatype
import exastencils.datastructures.ir.RealDatatype
import exastencils.datastructures.ir.VariableAccess
import exastencils.datastructures.ir.VariableDeclarationStatement
import exastencils.datastructures.ir.iv.FieldData
import exastencils.util.EvaluationException
import exastencils.util.SimplifyExpression

object SimplifyFloatExpressions extends DefaultStrategy("Simplify floating expressions") {

  private final val DEBUG : Boolean = false

  this += new Transformation("optimize", {
    case d @ VariableDeclarationStatement(RealDatatype(), _, Some(expr)) =>
      d.expression = Some(simplify(expr))
      d

    case a @ AssignmentStatement(VariableAccess(_, Some(RealDatatype())), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(VariableAccess(_, Some(PointerDatatype(RealDatatype()))), _), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(VariableAccess(_, Some(ArrayDatatype(RealDatatype(), _))), _), src, op) =>
      a.src = simplify(src)
      a

    case a @ AssignmentStatement(ArrayAccess(fd : FieldData, _), src, op) //
    if (fd.field.dataType.resolveUnderlyingDatatype == RealDatatype()) =>
      a.src = simplify(src)
      a
  })

  private final def simplify(expr : Expression) : Expression = {
    try {
      return SimplifyExpression.simplifyFloatingExpr(expr)
    } catch {
      case x : EvaluationException =>
        {
          if (DEBUG)
            Logger.debug("[simplify]  cannot simplify float expression: " + x.msg)
        }
        return expr
    }
  }
}
