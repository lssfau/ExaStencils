package exastencils.grid.ir

import exastencils.knowledge.ir._

/// IR_VirtualFieldCollection

object IR_VirtualFieldCollection extends IR_LeveledKnowledgeCollection[IR_VirtualField] {
  exastencils.core.Duplicate.registerConstant(this)

//  IR_UnfoldLeveledDeclarations.strategies += IR_UnfoldVirtualFieldDeclarations
//  IR_PrepareDeclarations.strategies += IR_PrepareVirtualFieldDeclaration
//  IR_PrepareAccesses.strategies += IR_PrepareVirtualFieldAccesses
//  IR_ProcessDeclarations.strategies += IR_ProcessVirtualFieldDeclarations
//  IR_ResolveAccesses.strategies += IR_ResolveVirtualFieldAccesses
}
