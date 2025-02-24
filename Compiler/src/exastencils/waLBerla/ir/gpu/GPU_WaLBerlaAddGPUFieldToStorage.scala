package exastencils.waLBerla.ir.gpu

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField

case class GPU_WaLBerlaAddGPUFieldToStorage(wbFields : IR_WaLBerlaField*) extends IR_Statement with IR_SpecialExpandable {
  if (!wbFields.forall(_.name == wbFields.head.name))
    Logger.error("\"GPU_WaLBerlaAddGPUFieldToStorage\" used incorrectly. Assumes fields with identical name but potentially different slots and levels.")

  def blockForest = IR_WaLBerlaBlockForest()

  def expandSpecial() = {
    var body : ListBuffer[IR_Statement] = ListBuffer()

    wbFields.sortBy(_.level) foreach (leveledField => {
      def refArrayToInit(slot : IR_Expression) = IR_WaLBerlaBlockDataID(leveledField, slot, onGPU = true)

      (0 until leveledField.numSlots).map(slot =>
        body += IR_Assignment(
          refArrayToInit(slot),
          leveledField.addToStorageGPU(blockForest.resolveMemberBaseAccess(), slot, IR_WaLBerlaBlockDataID(leveledField, slot, onGPU = false))))
    })

    IR_Scope(body)
  }
}
