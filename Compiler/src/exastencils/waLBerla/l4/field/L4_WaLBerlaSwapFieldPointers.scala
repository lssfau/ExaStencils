package exastencils.waLBerla.l4.field

import exastencils.base.ir.IR_Statement
import exastencils.base.l4.L4_Access
import exastencils.base.l4.L4_Statement
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.fieldlike.l4.L4_FieldLikeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldAccess
import exastencils.waLBerla.ir.field.IR_WaLBerlaSwapFieldPointers

case class L4_WaLBerlaSwapFieldPointers(
    var srcAcc : L4_Access,
    var dstAcc : L4_Access
) extends L4_Statement {

  override def progress : IR_Statement = {

    def resolveAccess(field : L4_Access) = field match {
      case access : L4_WaLBerlaFieldAccess => access
      case _                       => Logger.error("\"waLBerlaSwapPtr\": Passed argument is not a field access")
    }

    val src = resolveAccess(srcAcc)
    val dst = resolveAccess(dstAcc)
    if (!(L4_WaLBerlaFieldCollection.contains(src) && L4_WaLBerlaFieldCollection.contains(dst)))
      Logger.error("\"waLBerlaSwapPtr\" accepts two waLBerla field accesses as arguments.")

    val wbSrc = L4_WaLBerlaFieldCollection.getByFieldAccess(src).get.progress()
    val wbDst = L4_WaLBerlaFieldCollection.getByFieldAccess(dst).get.progress()

    IR_WaLBerlaSwapFieldPointers(
      IR_WaLBerlaFieldAccess(wbSrc, L4_FieldLikeAccess.resolveSlot(wbSrc, src.slot), IR_LoopOverDimensions.defIt(wbSrc.numDimsGrid)),
      IR_WaLBerlaFieldAccess(wbDst, L4_FieldLikeAccess.resolveSlot(wbDst, dst.slot), IR_LoopOverDimensions.defIt(wbDst.numDimsGrid)))
  }

  override def prettyprint(out : PpStream) : Unit =
    out << "waLBerlaSwapPtr(" << srcAcc << "," << dstAcc << ")"
}
