package exastencils.stencil.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldAccess
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream

/// IR_StencilAccess

case class IR_StencilAccess(var stencil : IR_Stencil) extends IR_Expression {
  override def datatype = stencil.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
}

/// IR_StencilFieldAccess

case class IR_StencilFieldAccess(var stencilFieldSelection : StencilFieldSelection, var index : IR_ExpressionIndex) extends IR_Expression {
  override def datatype = stencilFieldSelection.stencilField.stencil.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

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
