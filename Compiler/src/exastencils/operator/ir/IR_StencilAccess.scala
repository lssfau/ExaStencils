package exastencils.operator.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_StencilFieldSelection

/// IR_StencilAccess

case class IR_StencilAccess(var target : IR_Stencil, var offset : Option[IR_ConstIndex]) extends IR_OperatorAccess with IR_SpecialExpandable {
  override def datatype = target.datatype
  override def assembleOffsetMap() = target.assembleOffsetMap()
  override def stencil = target
}

/// IR_StencilFieldAccess

case class IR_StencilFieldAccess(var selection /* FIXME: target : IR_StencilField */ : IR_StencilFieldSelection, var index : IR_ExpressionIndex, var offset : Option[IR_ConstIndex]) extends IR_OperatorAccess with IR_SpecialExpandable {
  // FIXME: currently returns array dt
  override def datatype = selection.stencilField.field.fieldLayout.datatype
  override def target = selection.stencilField
  override def assembleOffsetMap() = selection.stencilField.stencil.assembleOffsetMap()
  override def stencil = target.stencil
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
