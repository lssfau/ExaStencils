package exastencils.domain.ir

import exastencils.knowledge.ir.IR_KnowledgeCollection

/// IR_DomainCollection

object IR_DomainCollection extends IR_KnowledgeCollection[IR_Domain] {
  exastencils.core.Duplicate.registerConstant(this)
}
