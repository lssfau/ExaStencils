package exastencils.field.l4

import exastencils.field.ir._
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._

/// L4_FieldCollection

object L4_FieldCollection extends L4_LeveledKnowledgeCollection[L4_Field, IR_Field] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareFieldDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessFieldDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareFieldAccesses
  L4_ResolveAccesses.strategies += L4_ResolveFieldAccesses

  override def name = "L4_FieldCollection"
  override def progress() = objects.foreach(obj => IR_FieldCollection.add(obj.progress()))
}
