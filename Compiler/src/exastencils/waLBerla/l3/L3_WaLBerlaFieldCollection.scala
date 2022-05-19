package exastencils.waLBerla.l3

import scala.collection.mutable.HashMap

import exastencils.base.l3.L3_Datatype
import exastencils.base.l4.L4_ConstIndex
import exastencils.config.Knowledge
import exastencils.fieldlike.l3.L3_FieldLikeCollection
import exastencils.fieldlike.l3.L3_FieldLikeCollections
import exastencils.fieldlike.l4.L4_FieldLikeLayout
import exastencils.grid.l3.L3_Localization
import exastencils.knowledge.l3._
import exastencils.waLBerla.l4.field._


/// L3_WaLBerlaFieldCollection

object L3_WaLBerlaFieldCollection extends L3_FieldLikeCollection[L3_WaLBerlaField, L4_WaLBerlaField] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_FieldLikeCollections.register(this)
  L3_KnowledgeContainer.register(this)

  override def name = "L3_WaLBerlaFieldCollection"
  override def progress() = {
    prepareFieldLayouts()
    objects.foreach(obj => L4_WaLBerlaFieldCollection.add(obj.progress()))
  }

  override def prepareFieldLayouts() : Unit = {
    var requiredLayouts = HashMap[(String, Int), (L3_Datatype, L3_Localization)]()
    for (field <- objects)
      requiredLayouts += ((field.fieldLayoutName, field.level) -> (field.datatype, field.localization))

    def defIndex = L4_ConstIndex(Array.fill(Knowledge.dimensionality - 1)(0))

    for (layout <- requiredLayouts.toList.sortBy(_._1._2).sortBy(_._1._1)) {
      // generate layout
      val genLayout = L4_WaLBerlaFieldLayout(
        layout._1._1, // name
        layout._1._2, // level
        Knowledge.dimensionality, // dims
        layout._2._1.progress, // datatype
        "fzyx", // waLBerla layout
        L4_FieldLikeLayout.default_ghostLayers(layout._2._2.progress), // to be determined later
        false,
        L4_FieldLikeLayout.default_duplicateLayers(layout._2._2.progress),
        false,
        defIndex)

      // register layout
      L4_WaLBerlaFieldLayoutCollection.add(genLayout)
    }
  }
}