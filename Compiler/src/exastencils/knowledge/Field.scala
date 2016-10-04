package exastencils.knowledge

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures._
import exastencils.field.ir._

@deprecated("to be integrated into IR_FieldAccess and/or replaced by the new accessor classes", "04.10.16")
case class FieldSelection(
    var field : IR_Field,
    var level : IR_Expression,
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

