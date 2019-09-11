package exastencils.operator.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures._

/// IR_StencilFieldAccess

object IR_StencilFieldAccess {
  def apply(stencilField : IR_StencilField, slot : IR_Expression, index : IR_ExpressionIndex, offset : Option[IR_ConstIndex])
  = new IR_StencilFieldAccess(stencilField, slot, IR_LoopOverFragments.defIt, index, offset)
}

case class IR_StencilFieldAccess(
    var stencilField : IR_StencilField,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var index : IR_ExpressionIndex,
    var offset : Option[IR_ConstIndex]) extends IR_OperatorAccess with IR_SpecialExpandable {

  override def target = stencilField
  // FIXME: currently returns array dt
  override def datatype = stencilField.field.layout.datatype
  override def assembleOffsetMap() = stencilField.stencil.assembleOffsetMap()
  override def stencil = stencilField.stencil

  // shortcuts
  def field = stencilField.field
}

/// IR_ApplyOffsetToStencilFieldAccess

object IR_ApplyOffsetToStencilFieldAccess extends DefaultStrategy("Apply offsets to StencilFieldAccess nodes") {
  this += new Transformation("Resolve", {
    case access : IR_StencilFieldAccess if access.offset.isDefined =>
      for (i <- 0 until access.offset.get.size)
        access.index(i) += access.offset.get(i)
      access.offset = None
      access
  })
}
