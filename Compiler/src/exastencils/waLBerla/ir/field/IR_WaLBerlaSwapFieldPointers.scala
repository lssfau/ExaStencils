package exastencils.waLBerla.ir.field

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.logger.Logger

case class IR_WaLBerlaSwapFieldPointers(
    var src : IR_WaLBerlaFieldAccess,
    var dst : IR_WaLBerlaFieldAccess,
) extends IR_Statement with IR_Expandable {

  private def toWbData(wbfAcc : IR_WaLBerlaFieldAccess, onGPU : Boolean) = IR_IV_WaLBerlaGetField(wbfAcc.field, wbfAcc.slot, onGPU, IR_LoopOverFragments.defIt)

  if (IR_WaLBerlaFieldCollection.getByIdentifier(src.name, src.level).isEmpty || IR_WaLBerlaFieldCollection.getByIdentifier(dst.name, dst.level).isEmpty)
    Logger.error("\"IR_WaLBerlaSwapFieldPointers\": Both fields must be waLBerla fields")

  override def expand() = {
    def swapPointers(onGPU : Boolean) = IR_ExpressionStatement(IR_MemberFunctionCallArrow(toWbData(src, onGPU), "swapDataPointers", toWbData(dst, onGPU)))

    if (Knowledge.cuda_enabled)
      ListBuffer[IR_Statement](swapPointers(true), swapPointers(false)) // simply swap both CPU & GPU pointers
    else
      ListBuffer[IR_Statement](swapPointers(false))
  }
}
