package exastencils.operator.l4

import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection
import exastencils.logger.Logger

object L4_StencilTemplateCollection extends L4_LeveledKnowledgeCollection[L4_StencilTemplate, IR_KnowledgeObject] {
  exastencils.core.Duplicate.registerConstant(this)

  def progress() = if (objects.nonEmpty) Logger.warn(s"Ignoring ${ objects.length } unprogressable stencil templates from l4")
}

