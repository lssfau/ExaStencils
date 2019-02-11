package exastencils.field.l2

import exastencils.field.l3._
import exastencils.knowledge.l2.L2_KnowledgeContainer.L2_ProcessDeclarations
import exastencils.knowledge.l2._

/// L2_FieldCombinationCollection

object L2_FieldCombinationCollection extends L2_LeveledKnowledgeCollection[L2_FieldCombination, L3_FieldCombination] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_KnowledgeContainer.register(this)

  L2_ProcessDeclarations.strategies += L2_ProcessFieldCombinationDeclarations

  override def name = "L3_FieldCombinationCollection"
  override def progress() = objects.foreach(obj => L3_FieldCombinationCollection.add(obj.progress()))

  def existsInCombination(field : L2_Field) = { objects.exists(_.fields.contains(field)) }
  def existsInCombination(field : L2_Field, combType : String) = { objects.exists(c => combType == c.combinationType && c.fields.contains(field)) }

  def getByFieldInCombination(field : L2_Field) = { objects.filter(_.fields.contains(field)) }
  def getByFieldInCombination(field : L2_Field, combType : String) = { objects.filter(c => combType == c.combinationType && c.fields.contains(field)) }
}
