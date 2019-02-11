package exastencils.field.l4

import exastencils.field.ir._
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ProcessDeclarations
import exastencils.knowledge.l4._

/// L4_FieldCombinationCollection

object L4_FieldCombinationCollection extends L4_LeveledKnowledgeCollection[L4_FieldCombination, IR_FieldCombination] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_ProcessDeclarations.strategies += L4_ProcessFieldCombinationDeclarations

  override def name = "L4_FieldCombinationCollection"
  override def progress() = objects.foreach(obj => IR_FieldCombinationCollection.add(obj.progress()))

  def existsInCombination(field : L4_Field) = { objects.exists(_.fields.contains(field)) }
  def existsInCombination(field : L4_Field, combType : String) = { objects.exists(c => combType == c.combinationType && c.fields.contains(field)) }

  def getByFieldInCombination(field : L4_Field) = { objects.filter(_.fields.contains(field)) }
  def getByFieldInCombination(field : L4_Field, combType : String) = { objects.filter(c => combType == c.combinationType && c.fields.contains(field)) }
}
