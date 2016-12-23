package exastencils.deprecated.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures.Node
import exastencils.field.ir.IR_Field

/// IR_FieldSelection

@deprecated("to be integrated into IR_FieldAccess and/or replaced by the new accessor classes", "04.10.16")
case class IR_FieldSelection(
    var field : IR_Field,
    var level : Int,
    var slot : IR_Expression,
    var arrayIndex : Option[Int] = None, // TODO: delete
    var fragIdx : IR_Expression = IR_LoopOverFragments.defIt) extends Node {

  // shortcuts to Field members
  def codeName = field.codeName
  def fieldLayout = field.fieldLayout
  def referenceOffset = field.referenceOffset

  // other shortcuts
  def domainIndex = field.domain.index
}

