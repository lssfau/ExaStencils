package exastencils.stencil.l3

import exastencils.field.l4.L4_FieldCollection
import exastencils.knowledge.l3.L3_LeveledKnowledgeCollection
import exastencils.stencil.l4._

///// L3_StencilTemplateCollection
//
//object L3_StencilTemplateCollection extends L3_LeveledKnowledgeCollection[L3_StencilTemplate] {
//  def progress() = {
//    for (obj <- objects) {
//      val (stencilField, field, stencil) = obj.progress
//      L4_FieldCollection.add(field)
//      L4_StencilTemplateCollection.add(stencil)
//      L4_StencilFieldCollection.add(stencilField)
//    }
//  }
//}
