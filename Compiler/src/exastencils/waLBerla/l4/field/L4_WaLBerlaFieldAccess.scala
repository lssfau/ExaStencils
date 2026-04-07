package exastencils.waLBerla.l4.field

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.field.l4.L4_SlotSpecification
import exastencils.fieldlike.l4.L4_FieldLikeAccess
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldAccess

/// L4_WaLBerlaFieldAccess

case class L4_WaLBerlaFieldAccess(
    var target : L4_WaLBerlaField,
    var slot : L4_SlotSpecification,
    var offset : Option[L4_ConstIndex] = None,
    var frozen : Boolean = false,
    var matIndex : Option[L4_MatIndex] = None
) extends L4_FieldLikeAccess {

  override def progress : IR_WaLBerlaFieldAccess = ProgressLocation {
    val field = target.getProgressedObj()

    val numDims = field.layout.numDimsGrid
    val index = IR_LoopOverDimensions.defIt(numDims)

    val progOffset = if (offset.isDefined) {
      val progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < index.length) progressedOffset.indices :+= 0
      Some(progressedOffset)
    } else {
      None
    }

    new IR_WaLBerlaFieldAccess(field, L4_FieldLikeAccess.resolveSlot(field, slot), IR_LoopOverFragments.defIt, index,
      progOffset, frozen, if (matIndex.isDefined) Some(matIndex.get.progress) else None)
  }
}
