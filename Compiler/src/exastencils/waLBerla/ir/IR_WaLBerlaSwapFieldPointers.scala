package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Statement
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.prettyprinting.PrettyPrintable

case class IR_WaLBerlaSwapFieldPointers(
    var src : IR_WaLBerlaField,
    var dst : IR_WaLBerlaField
) extends IR_Statement with PrettyPrintable {

  private def toWbData(wbf : IR_WaLBerlaField) = IR_IV_WaLBerlaFieldData(wbf, IR_LoopOverFragments.defIt)
  val srcData = toWbData(src)
  val dstData = toWbData(dst)

  if (IR_WaLBerlaFieldCollection.getByIdentifier(src.name, src.level).isEmpty || IR_WaLBerlaFieldCollection.getByIdentifier(dst.name, dst.level).isEmpty)
    Logger.error("\"IR_WaLBerlaSwapFieldPointers\": Both fields must be waLBerla fields")

  override def prettyprint(out : PpStream) : Unit = {
    out << srcData << "->swapDataPointers(" << dstData << ");"
  }
}
