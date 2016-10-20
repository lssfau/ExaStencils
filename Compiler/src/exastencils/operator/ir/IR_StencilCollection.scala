package exastencils.operator.ir

import exastencils.knowledge.ir.IR_LeveledKnowledgeCollection

/// IR_StencilCollection

object IR_StencilCollection extends IR_LeveledKnowledgeCollection[IR_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)
}
