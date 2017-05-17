package exastencils.field.l2

import exastencils.field.l3.L3_Field
import exastencils.knowledge.l2._
import exastencils.knowledge.l3.L3_FieldCollection

/// L2_FieldCollection

object L2_FieldCollection extends L2_LeveledKnowledgeCollection[L2_Field, L3_Field] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_UnfoldLeveledDeclarations.strategies += L2_UnfoldFieldDeclarations
  L2_PrepareDeclarations.strategies += L2_PrepareFieldDeclaration
  L2_PrepareAccesses.strategies += L2_PrepareFieldAccesses
  L2_ProcessDeclarations.strategies += L2_ProcessFieldDeclarations
  L2_ResolveAccesses.strategies += L2_ResolveFieldAccesses

  def progress() = objects.foreach(obj => L3_FieldCollection.add(obj.progress()))
}
