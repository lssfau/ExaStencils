package exastencils.operator.l2

import exastencils.knowledge.l2._
import exastencils.operator.l3._

/// L2_StencilTemplateCollection

object L2_StencilTemplateCollection extends L2_LeveledKnowledgeCollection[L2_StencilTemplate, L3_StencilTemplate] {
  def progress() = objects.foreach(obj => L3_StencilTemplateCollection.add(obj.progress()))
}
