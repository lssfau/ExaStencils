package exastencils.waLBerla.ir.gpu

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.interfacing._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_BlockDataID

case class GPU_WaLBerlaSetGPUFlag(field : IR_WaLBerlaField) extends IR_WaLBerlaWrapperFunction {
  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    val lvl = IR_FunctionArgument("lvl", IR_IntegerDatatype)
    val slot = IR_FunctionArgument("slot", IR_IntegerDatatype)
    val onDevice = IR_FunctionArgument("onDevice", IR_BooleanDatatype)

    val deviceUpdated = GPU_WaLBerlaDeviceDataUpdated(field, slot.access, lvl.access, IR_WaLBerlaLoopOverLocalBlocks.defIt)
    val hostUpdated = GPU_WaLBerlaHostDataUpdated(field, slot.access, lvl.access, IR_WaLBerlaLoopOverLocalBlocks.defIt)

    val body : ListBuffer[IR_Statement] = ListBuffer()

    body += IR_WaLBerlaLoopOverLocalBlocks(
      IR_IfCondition(onDevice.access,
        IR_Assignment(deviceUpdated, true),
        IR_Assignment(hostUpdated, true)))

    IR_WaLBerlaPlainFunction(name, WB_BlockDataID, ListBuffer(lvl, slot, onDevice), body)
  }

  override def name : String = s"markHostOrDeviceDataAsUpdated_${field.name}"
  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true
}
