package exastencils.stencil.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_StencilFieldSelection
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.operator.ir._

/// IR_StencilAccess

case class IR_StencilAccess(var stencil : IR_Stencil, var offset : Option[IR_ExpressionIndex]) extends IR_Expression with IR_SpecialExpandable {
  override def datatype = stencil.datatype
}

/// IR_StencilFieldAccess

case class IR_StencilFieldAccess(var stencilFieldSelection : IR_StencilFieldSelection, var index : IR_ExpressionIndex, var offset : Option[IR_ExpressionIndex]) extends IR_Expression with IR_SpecialExpandable {
  // FIXME: currently returns array dt
  override def datatype = stencilFieldSelection.stencilField.field.fieldLayout.datatype

  def buildStencil : IR_Stencil = {
    if (offset.isDefined)
      Logger.warn(s"IR_StencilFieldAccess with unresolved offset ${ offset.get.prettyprint() } found")

    var entries : ListBuffer[IR_StencilEntry] = ListBuffer()
    for (e <- stencilFieldSelection.offsets.indices) {
      val stencilFieldIdx = Duplicate(index)
      stencilFieldIdx(stencilFieldSelection.stencilField.field.fieldLayout.numDimsData - 1) = e // TODO: assumes last index is vector dimension
      val fieldSel = stencilFieldSelection.toFieldSelection
      entries += IR_StencilEntry(stencilFieldSelection.offsets(e), IR_FieldAccess(fieldSel, stencilFieldIdx, None))
    }
    IR_Stencil("GENERATED_PLACEHOLDER_STENCIL", stencilFieldSelection.field.level, entries)
  }
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
