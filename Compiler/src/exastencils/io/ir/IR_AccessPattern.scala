package exastencils.io.ir

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex

object IR_AccessPattern {
  def apply(accessCallback : IR_Expression => IR_Access) : IR_AccessPattern = new IR_AccessPattern(accessFunction = accessCallback, accessIndices = None, stridePerDimension = None)
}

// TODO
case class IR_AccessPattern(var accessFunction : IR_Expression => IR_Access, var accessIndices : Option[Array[IR_ExpressionIndex]], var stridePerDimension : Option[Array[IR_Expression]]) {

}

