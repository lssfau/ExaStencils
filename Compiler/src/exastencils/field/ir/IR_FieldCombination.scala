package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate
import exastencils.knowledge.ir._

/// IR_FieldCombination

case class IR_FieldCombination(
    var name : String,
    var level : Int,
    var combinationType : String,
    var fields : ListBuffer[IR_Field]) extends IR_LeveledKnowledgeObject {

  override def createDuplicate() : IR_FieldCombination = {
    IR_FieldCombination.tupled(Duplicate(IR_FieldCombination.unapply(this).get))
  }
}
