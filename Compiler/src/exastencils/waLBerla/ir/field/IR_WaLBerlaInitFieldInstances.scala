package exastencils.waLBerla.ir.field

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks
import exastencils.waLBerla.ir.interfacing._

case class IR_WaLBerlaInitFieldInstances(onGPU : Boolean, wbFields : IR_WaLBerlaField*) extends IR_WaLBerlaFuturePlainFunction {
  override def name : String = s"initFieldInstances_${ wbFields.head.name }" + (if (onGPU) "_onGPU" else "")
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint

  override def isInterfaceFunction : Boolean = true
  override def inlineImplementation : Boolean = true

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    var body = ListBuffer[IR_Statement]()

    // init pointers to waLBerla field datastructures in block loop via "getData" function
    for (wbf <- wbFields) {
      val slotIt = IR_VariableAccess("slotIt", IR_IntegerDatatype)
      var block = IR_WaLBerlaLoopOverBlocks.block
      var blockIdx = IR_WaLBerlaLoopOverBlocks.defIt
      val getField = IR_IV_WaLBerlaGetField(wbf, slotIt, onGPU, blockIdx)

      body += IR_ForLoop(IR_VariableDeclaration(slotIt, 0), slotIt < wbf.numSlots, IR_PreIncrement(slotIt),
        IR_Assignment(getField, block.getData(IR_WaLBerlaBlockDataID(wbf, slotIt, onGPU))))
    }

    body = ListBuffer(IR_WaLBerlaLoopOverBlocks(body, setupWaLBerlaFieldPointers = false))

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }
}
