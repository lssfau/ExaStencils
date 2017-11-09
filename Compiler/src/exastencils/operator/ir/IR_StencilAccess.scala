package exastencils.operator.ir

import exastencils.base.ir._

/// IR_StencilAccess

case class IR_StencilAccess(var target : IR_Stencil, var offset : Option[IR_ConstIndex]) extends IR_OperatorAccess with IR_SpecialExpandable {
  override def datatype = target.datatype
  override def assembleOffsetMap() = target.assembleOffsetMap()
  override def stencil = target
}
