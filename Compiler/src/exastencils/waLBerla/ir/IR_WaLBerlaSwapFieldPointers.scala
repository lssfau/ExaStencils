package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Statement
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.prettyprinting.PrettyPrintable

case class IR_WaLBerlaSwapFieldPointers(
    var src : IR_WaLBerlaFieldAccess,
    var dst : IR_WaLBerlaFieldAccess,
) extends IR_Statement with PrettyPrintable {

  private def toWbData(wbfAcc : IR_WaLBerlaFieldAccess) = IR_IV_WaLBerlaFieldData(wbfAcc.field, wbfAcc.slot, IR_LoopOverFragments.defIt)

  if (IR_WaLBerlaFieldCollection.getByIdentifier(src.name, src.level).isEmpty || IR_WaLBerlaFieldCollection.getByIdentifier(dst.name, dst.level).isEmpty)
    Logger.error("\"IR_WaLBerlaSwapFieldPointers\": Both fields must be waLBerla fields")

  override def prettyprint(out : PpStream) : Unit = {
    out << toWbData(src) << "->swapDataPointers(" << toWbData(dst) << ");"
  }
}
