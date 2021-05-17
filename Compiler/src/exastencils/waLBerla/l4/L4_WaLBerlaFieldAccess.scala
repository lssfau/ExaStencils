package exastencils.waLBerla.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_Expression
import exastencils.base.l4.L4_CanBeOffset
import exastencils.base.l4.L4_ConstIndex
import exastencils.base.l4.L4_Index
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.field.l4.L4_FieldAccess
import exastencils.field.l4.L4_SlotSpecification
import exastencils.knowledge.l4.L4_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaFieldAccess

case class L4_WaLBerlaFieldAccess(
    var target : L4_WaLBerlaField,
    var slot : L4_SlotSpecification,
    var offset : Option[L4_ConstIndex] = None,
    var arrayIndex : Option[Int] = None,
    var frozen : Boolean = false,
    var matIndex : Option[Array[L4_Index]] = None
) extends L4_LeveledKnowledgeAccess with L4_CanBeOffset {

  // reuse most of field access impl
  val fieldAccessL4 = L4_FieldAccess(target.field, slot, offset, arrayIndex, frozen, matIndex)

  override def progress : IR_Expression = ProgressLocation {
    val fAcc = fieldAccessL4.progress
    IR_WaLBerlaFieldAccess(target.progress(), fAcc.slot, IR_LoopOverFragments.defIt, fAcc.index, fAcc.offset, frozen, if(matIndex.isDefined) Some(matIndex.get.map(midx => midx.progress)) else None)
  }

  override def prettyprint(out : PpStream) : Unit = {
    out << fieldAccessL4.prettyprint()
  }
}
