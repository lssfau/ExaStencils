package exastencils.operator.l3

import scala.collection.mutable.HashMap

import exastencils.base.l3.L3_Datatype
import exastencils.base.l4.L4_ConstIndex
import exastencils.baseExt.l3.L3_ArrayDatatype
import exastencils.config.Knowledge
import exastencils.field.l4._
import exastencils.knowledge.l3.L3_LeveledKnowledgeCollection
import exastencils.operator.l4._
import exastencils.stencil.l4.L4_StencilFieldCollection

/// L3_StencilTemplateCollection

object L3_StencilTemplateCollection extends L3_LeveledKnowledgeCollection[L3_StencilTemplate, L4_StencilTemplate] {
  def progress() = {
    prepareFieldLayouts()

    for (obj <- objects) {
      val field = obj.progressField()
      L4_FieldCollection.add(field)
      val stencilField = obj.progressStencilField(field)
      L4_StencilFieldCollection.add(stencilField)
      val stencilTemplate = obj.progress()
      L4_StencilTemplateCollection.add(stencilTemplate)
    }
  }

  def prepareFieldLayouts() : Unit = {
    // name, level -> (array)datatype, localization
    var requiredLayouts = HashMap[(String, Int), (L3_Datatype, String)]()
    for (obj <- objects)
      requiredLayouts +=
        ((obj.fieldLayoutName, obj.level) -> (L3_ArrayDatatype(obj.resolveCoeffDataType, obj.offsets.length), obj.localization))

    def defIndex = L4_ConstIndex(Array.fill(Knowledge.dimensionality)(0))

    for (layout <- requiredLayouts.toList.sortBy(_._1._2).sortBy(_._1._1)) {
      L4_FieldLayoutCollection.add(L4_FieldLayout(
        layout._1._1, // name
        layout._1._2, // level
        Knowledge.dimensionality, // FIXME: numDimsGrid
        layout._2._1.progress, // datatype
        layout._2._2, // localization
        L4_FieldLayout.default_ghostLayers(layout._2._2), // to be determined later
        false,
        L4_FieldLayout.default_duplicateLayers(layout._2._2),
        false,
        defIndex))
    }
  }
}
