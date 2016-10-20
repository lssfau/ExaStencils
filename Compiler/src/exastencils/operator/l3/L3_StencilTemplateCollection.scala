package exastencils.operator.l3

import exastencils.knowledge.l3.L3_LeveledKnowledgeCollection
import exastencils.operator.l4._

/// L3_StencilTemplateCollection

object L3_StencilTemplateCollection extends L3_LeveledKnowledgeCollection[L3_StencilTemplate, L4_StencilTemplate] {
  def progress() = {
    for (obj <- objects) {
      L4_StencilTemplateCollection.add(obj.progress())
      //      val (stencilField, field, stencil) = obj.progress()
      //      L4_FieldCollection.add(field)
      //      L4_StencilTemplateCollection.add(stencil)
      //      L4_StencilFieldCollection.add(stencilField)
    }
  }
}
