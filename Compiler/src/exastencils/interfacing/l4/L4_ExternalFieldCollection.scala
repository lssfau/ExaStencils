package exastencils.interfacing.l4

import exastencils.interfacing.ir._
import exastencils.knowledge.l4._

/// L4_ExternalFieldCollection

object L4_ExternalFieldCollection extends L4_LeveledKnowledgeCollection[L4_ExternalField, IR_ExternalField] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

//  L4_PrepareDeclarations.strategies += L4_PrepareExternalFieldDeclarations
//  L4_ProcessDeclarations.strategies += L4_ProcessExternalFieldDeclarations
//
//  L4_PrepareAccesses.strategies += L4_PrepareExternalFieldAccesses
//  L4_ResolveAccesses.strategies += L4_ResolveExternalFieldAccesses

  override def name = "L4_ExternalFieldCollection"
  override def progress() = objects.foreach(obj => IR_ExternalFieldCollection.add(obj.progress()))
}
