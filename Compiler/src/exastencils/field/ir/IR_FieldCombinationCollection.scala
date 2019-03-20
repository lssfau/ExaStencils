package exastencils.field.ir

import exastencils.knowledge.ir._

/// IR_FieldCombinationCollection

object IR_FieldCombinationCollection extends IR_LeveledKnowledgeCollection[IR_FieldCombination] {
  exastencils.core.Duplicate.registerConstant(this)

  def existsInCombination(field : IR_Field) = { objects.exists(_.fields.contains(field)) }
  def existsInCombination(field : IR_Field, combType : String) = { objects.exists(c => combType == c.combinationType && c.fields.contains(field)) }

  def getByFieldInCombination(field : IR_Field) = { objects.filter(_.fields.contains(field)) }
  def getByFieldInCombination(field : IR_Field, combType : String) = { objects.filter(c => combType == c.combinationType && c.fields.contains(field)) }
}
