package exastencils.waLBerla.l4.field

import exastencils.fieldlike.l4.L4_FieldLikeLayoutAccess
import exastencils.fieldlike.l4.L4_FieldLikeLayoutCollection
import exastencils.fieldlike.l4.L4_FieldLikeLayoutCollections
import exastencils.knowledge.l4.L4_KnowledgeContainer
import exastencils.waLBerla.ir.field._

/// L4_WaLBerlaFieldLayoutCollection

object L4_WaLBerlaFieldLayoutCollection extends L4_FieldLikeLayoutCollection[L4_WaLBerlaFieldLayout, IR_WaLBerlaFieldLayout]  {

  exastencils.core.Duplicate.registerConstant(this)

  L4_FieldLikeLayoutCollections.register(this)
  L4_KnowledgeContainer.register(this)

  override def name = "L4_WaLBerlaFieldLayoutCollection"
  override def progress() = for (obj <- objects) IR_WaLBerlaFieldLayoutCollection.objects += obj.progress
}

/// L4_WaLBerlaFieldLayoutAccess

case class L4_WaLBerlaFieldLayoutAccess(var target : L4_WaLBerlaFieldLayout) extends L4_FieldLikeLayoutAccess[IR_WaLBerlaFieldLayout]