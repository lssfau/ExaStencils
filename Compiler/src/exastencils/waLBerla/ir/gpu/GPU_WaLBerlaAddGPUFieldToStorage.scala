package exastencils.waLBerla.ir.gpu

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField

case class GPU_WaLBerlaAddGPUFieldToStorage(wbField : IR_WaLBerlaField) extends IR_Statement with IR_SpecialExpandable {

  def blockForest = IR_WaLBerlaBlockForest()

  def expandSpecial() = {
    def refArrayToInit(slot : IR_Expression) = IR_WaLBerlaBlockDataID(wbField, slot, onGPU = true)

    var body : ListBuffer[IR_Statement] = ListBuffer()
    (0 until wbField.numSlots).map(slot =>
      body += IR_Assignment(
        refArrayToInit(slot),
        wbField.addToStorageGPU(blockForest.resolveMemberBaseAccess(), slot, IR_WaLBerlaBlockDataID(wbField, slot, onGPU = false))))

    IR_Scope(body)
  }
}
