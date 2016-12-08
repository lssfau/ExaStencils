package exastencils.deprecated.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures.Node
import exastencils.operator.ir.IR_StencilField

/// IR_StencilFieldSelection

@deprecated("to be integrated into IR_StencilFieldAccess and/or replaced by the new accessor classes", "04.10.16")
case class IR_StencilFieldSelection(
    var stencilField : IR_StencilField,
    var level : Int,
    var slot : IR_Expression,
    var arrayIndex : Option[Int],
    var fragIdx : IR_Expression = IR_LoopOverFragments.defIt) extends Node {

  def toFieldSelection = {
    IR_FieldSelection(field, level, slot, arrayIndex, fragIdx)
  }

  // shortcuts to stencilField members
  def field = stencilField.field
  def offsets = stencilField.offsets

  // shortcuts to Field members
  def codeName = field.codeName
  def fieldLayout = field.fieldLayout
  def referenceOffset = field.referenceOffset
}
