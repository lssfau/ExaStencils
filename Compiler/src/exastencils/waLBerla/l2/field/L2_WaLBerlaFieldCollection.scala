package exastencils.waLBerla.l2.field

import exastencils.fieldlike.l2.L2_FieldLikeCollection
import exastencils.fieldlike.l2.L2_FieldLikeCollections
import exastencils.knowledge.l2.L2_KnowledgeContainer
import exastencils.waLBerla.l3.L3_WaLBerlaField
import exastencils.waLBerla.l3.L3_WaLBerlaFieldCollection

object L2_WaLBerlaFieldCollection extends L2_FieldLikeCollection[L2_WaLBerlaField, L3_WaLBerlaField] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_FieldLikeCollections.register(this)
  L2_KnowledgeContainer.register(this)

  override def name = "L2_WaLBerlaFieldCollection"
  override def progress() = {
    objects.foreach(obj => L3_WaLBerlaFieldCollection.add(obj.progress()))
  }
}
