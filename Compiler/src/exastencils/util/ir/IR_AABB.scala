package exastencils.util.ir

import exastencils.base.ir._
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.prettyprinting._

/// IR_AABB

case class IR_AABB(var lower : IR_ExpressionIndex, var upper : IR_ExpressionIndex) extends IR_Node with PrettyPrintable {

  IR_ReplaceIntWithReal.applyStandalone(lower)
  IR_ReplaceIntWithReal.applyStandalone(upper)

  override def prettyprint(out : PpStream) = out << lower << " to " << upper

  def numDims = math.min(lower.length, upper.length)

  def width() = upper - lower
  def width(dim : Int) = upper(dim) - lower(dim)

  def widthAsDouble(dim : Int) = IR_SimplifyExpression.evalFloating(upper(dim) - lower(dim))
}
