package exastencils.waLBerla.l4

import exastencils.fieldlike.l4.L4_FieldLikeCollection
import exastencils.fieldlike.l4.L4_FieldLikeCollections
import exastencils.knowledge.l4.L4_KnowledgeContainer
import exastencils.waLBerla.ir.IR_WaLBerlaField
import exastencils.waLBerla.ir.IR_WaLBerlaFieldCollection

object L4_WaLBerlaFieldCollection extends L4_FieldLikeCollection[L4_WaLBerlaField, IR_WaLBerlaField] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_FieldLikeCollections.register(this)
  L4_KnowledgeContainer.register(this)

  override def name = "L4_WaLBerlaFieldCollection"
  override def progress() : Unit = objects.foreach(obj => IR_WaLBerlaFieldCollection.add(obj.progress()))
}
