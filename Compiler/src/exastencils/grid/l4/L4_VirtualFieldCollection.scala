package exastencils.grid.l4

import exastencils.grid.ir._
import exastencils.knowledge.l4._

/// L4_VirtualFieldCollection

object L4_VirtualFieldCollection extends L4_LeveledKnowledgeCollection[L4_VirtualField, IR_VirtualField] {
  exastencils.core.Duplicate.registerConstant(this)

//  L4_UnfoldLeveledDeclarations.strategies += L4_UnfoldVirtualFieldDeclarations
//  L4_PrepareDeclarations.strategies += L4_PrepareVirtualFieldDeclaration
//  L4_PrepareAccesses.strategies += L4_PrepareVirtualFieldAccesses
//  L4_ProcessDeclarations.strategies += L4_ProcessVirtualFieldDeclarations
//  L4_ResolveAccesses.strategies += L4_ResolveVirtualFieldAccesses

  def progress() = objects.foreach(obj => IR_VirtualFieldCollection.add(obj.progress()))
}
