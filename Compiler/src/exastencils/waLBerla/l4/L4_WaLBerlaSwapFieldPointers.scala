package exastencils.waLBerla.l4

import exastencils.base.ir.IR_Statement
import exastencils.base.l4.L4_Access
import exastencils.base.l4.L4_Statement
import exastencils.field.l4.L4_FieldAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaSwapFieldPointers

case class L4_WaLBerlaSwapFieldPointers(
    var srcAcc : L4_Access,
    var dstAcc : L4_Access
) extends L4_Statement {

  override def progress : IR_Statement = {

    def resolveAccess(field : L4_Access) = field match {
      case access : L4_FieldAccess => access
      case _                       => Logger.error("\"waLBerlaSwapPtr\": Passed argument is not a field access")
    }

    val src = resolveAccess(srcAcc)
    val dst = resolveAccess(dstAcc)
    if ( !(L4_WaLBerlaFieldCollection.contains(src) && L4_WaLBerlaFieldCollection.contains(dst)) )
      Logger.error("\"waLBerlaSwapPtr\" accepts two waLBerla field accesses as arguments.")

    IR_WaLBerlaSwapFieldPointers(src.progress, dst.progress)
  }

  override def prettyprint(out : PpStream) : Unit =
    out << "waLBerlaSwapPtr(" << srcAcc << "," << dstAcc << ")"
}
