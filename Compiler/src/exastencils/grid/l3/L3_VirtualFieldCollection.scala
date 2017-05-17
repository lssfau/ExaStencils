package exastencils.grid.l3

import exastencils.grid.l4._
import exastencils.knowledge.l3._

/// L3_VirtualFieldCollection

object L3_VirtualFieldCollection extends L3_LeveledKnowledgeCollection[L3_VirtualField, L4_VirtualField] {
  exastencils.core.Duplicate.registerConstant(this)

//  L3_UnfoldLeveledDeclarations.strategies += L3_UnfoldVirtualFieldDeclarations
//  L3_PrepareDeclarations.strategies += L3_PrepareVirtualFieldDeclaration
//  L3_PrepareAccesses.strategies += L3_PrepareVirtualFieldAccesses
//  L3_ProcessDeclarations.strategies += L3_ProcessVirtualFieldDeclarations
//  L3_ResolveAccesses.strategies += L3_ResolveVirtualFieldAccesses

  def progress() = objects.foreach(obj => L4_VirtualFieldCollection.add(obj.progress()))
}
