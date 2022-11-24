package exastencils.waLBerla.ir.field

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.logger.Logger

case class IR_WaLBerlaSwapFieldPointers(
    var src : IR_WaLBerlaFieldAccess,
    var dst : IR_WaLBerlaFieldAccess,
) extends IR_Statement with IR_Expandable {

  private def toWbData(wbfAcc : IR_WaLBerlaFieldAccess) = IR_IV_WaLBerlaGetField(wbfAcc.field, wbfAcc.slot, IR_LoopOverFragments.defIt)

  if (IR_WaLBerlaFieldCollection.getByIdentifier(src.name, src.level).isEmpty || IR_WaLBerlaFieldCollection.getByIdentifier(dst.name, dst.level).isEmpty)
    Logger.error("\"IR_WaLBerlaSwapFieldPointers\": Both fields must be waLBerla fields")

  override def expand() = IR_ExpressionStatement(IR_MemberFunctionCallArrow(toWbData(src), "swapDataPointers", toWbData(dst)))
}
