package exastencils.field.l4

import exastencils.field.ir._
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._

/// L4_FieldLayoutCollection

object L4_FieldLayoutCollection extends L4_LeveledKnowledgeCollection[L4_FieldLayout, IR_FieldLayout] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareFieldLayoutDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessFieldLayoutDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareFieldLayoutAccesses
  L4_ResolveAccesses.strategies += L4_ResolveFieldLayoutAccesses

  override def name = "L4_FieldLayoutCollection"
  override def progress() = for (obj <- objects) IR_FieldLayoutCollection.objects += obj.progress
}
