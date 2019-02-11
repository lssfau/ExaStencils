package exastencils.field.l3

import exastencils.field.l4._
import exastencils.knowledge.l3.L3_KnowledgeContainer.L3_ProcessDeclarations
import exastencils.knowledge.l3._

/// L3_FieldCombinationCollection

object L3_FieldCombinationCollection extends L3_LeveledKnowledgeCollection[L3_FieldCombination, L4_FieldCombination] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_ProcessDeclarations.strategies += L3_ProcessFieldCombinationDeclarations

  override def name = "L4_FieldCombinationCollection"
  override def progress() = objects.foreach(obj => L4_FieldCombinationCollection.add(obj.progress()))

  def existsInCombination(field : L3_Field) = { objects.exists(_.fields.contains(field)) }
  def existsInCombination(field : L3_Field, combType : String) = { objects.exists(c => combType == c.combinationType && c.fields.contains(field)) }

  def getByFieldInCombination(field : L3_Field) = { objects.filter(_.fields.contains(field)) }
  def getByFieldInCombination(field : L3_Field, combType : String) = { objects.filter(c => combType == c.combinationType && c.fields.contains(field)) }
}
