package exastencils.stencil.ir

import exastencils.knowledge.ir.IR_LeveledKnowledgeCollection

object IR_StencilCollection extends IR_LeveledKnowledgeCollection[IR_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)
}
