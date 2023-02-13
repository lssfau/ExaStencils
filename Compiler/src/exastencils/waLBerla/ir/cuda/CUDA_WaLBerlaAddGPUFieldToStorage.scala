package exastencils.waLBerla.ir.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaCollection
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaFuturePlainFunction
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaPlainFunction

case class CUDA_WaLBerlaAddGPUFieldToStorage(wbFields : IR_WaLBerlaField*) extends IR_WaLBerlaFuturePlainFunction {

  def blockForest = IR_WaLBerlaBlockForest()
  def blocks = blockForest.ctorParameter

  if (!wbFields.forall(_.name == wbFields.head.name))
    Logger.error("\"IR_WaLBerlaAddGPUFieldToStorage\" used incorrectly. Assumes fields with identical name but potentially different slots and levels.")

  override def isInterfaceFunction : Boolean = false
  override def inlineImplementation : Boolean = false

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    // add deps
    IR_WaLBerlaCollection.get.addExternalDependency("cuda/AddGPUFieldToStorage.h")
    IR_WaLBerlaCollection.get.addExternalDependency("cuda/FieldCopy.h")

    var cpuBlockDataIDParam = IR_WaLBerlaBlockDataID(wbFields.head, slot = 0, onGPU = false)

    var params : ListBuffer[IR_FunctionArgument] = ListBuffer()
    params += blocks
    params += cpuBlockDataIDParam.ctorParameter

    val init = wbFields.sortBy(_.level).flatMap(leveledField => {
      (0 until leveledField.numSlots).map(slot =>
        leveledField.addToStorageGPU(blocks.access, slot, IR_WaLBerlaBlockDataID(leveledField, slot, onGPU = false)))
    })

    var body : ListBuffer[IR_Statement] = ListBuffer()

    body += IR_Return(IR_InitializerList(init : _*))

    val returnType = cpuBlockDataIDParam.datatype

    IR_WaLBerlaPlainFunction(name, returnType, params, body)
  }

  override def prettyprint_decl() : String = prettyprint()
  override def name : String = s"addToStorage_${ wbFields.head.name }_GPU"
  override def name_=(newName : String) : Unit = name = newName
}
