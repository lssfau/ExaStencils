package exastencils.stencil.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.deprecated.ir.IR_StencilFieldSelection
import exastencils.field.ir.IR_FieldAccess
import exastencils.operator.ir.IR_Stencil
import exastencils.operator.ir.IR_StencilEntry

/// IR_StencilAccess

case class IR_StencilAccess(var stencil : IR_Stencil) extends IR_Expression with IR_SpecialExpandable {
  override def datatype = stencil.datatype
}

/// IR_StencilFieldAccess

case class IR_StencilFieldAccess(var stencilFieldSelection : IR_StencilFieldSelection, var index : IR_ExpressionIndex) extends IR_Expression with IR_SpecialExpandable {
  override def datatype = stencilFieldSelection.stencilField.stencil.datatype

  def buildStencil : IR_Stencil = {
    var entries : ListBuffer[IR_StencilEntry] = ListBuffer()
    for (e <- stencilFieldSelection.stencil.entries.indices) {
      val stencilFieldIdx = Duplicate(index)
      stencilFieldIdx(stencilFieldSelection.stencilField.field.fieldLayout.numDimsData - 1) = e // TODO: assumes last index is vector dimension
      val fieldSel = stencilFieldSelection.toFieldSelection
      fieldSel.arrayIndex = Some(e)
      entries += IR_StencilEntry(stencilFieldSelection.stencil.entries(e).offset, IR_FieldAccess(fieldSel, stencilFieldIdx))
    }
    IR_Stencil("GENERATED_PLACEHOLDER_STENCIL", stencilFieldSelection.stencil.level, entries)
  }
}
