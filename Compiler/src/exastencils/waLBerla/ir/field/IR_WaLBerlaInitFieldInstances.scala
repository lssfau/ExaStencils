package exastencils.waLBerla.ir.field

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlockArray
import exastencils.waLBerla.ir.interfacing._

object IR_WaLBerlaInitFieldInstances {
  def initRoutine(onGPU : Boolean, wbf : IR_WaLBerlaField) : IR_Statement = {
    val slotIt = IR_VariableAccess("slotIt", IR_IntegerDatatype)
    var block = IR_WaLBerlaLoopOverLocalBlocks.block
    var fragIdx = IR_WaLBerlaLoopOverLocalBlocks.defIt
    val getField = IR_IV_WaLBerlaGetField(wbf, slotIt, onGPU, fragIdx)

    val assign = IR_Assignment(getField, block.getData(IR_WaLBerlaBlockDataID(wbf, slotIt, onGPU)))

    def wrapAroundSlotLoop(stmts : IR_Statement*) =
      IR_ForLoop(IR_VariableDeclaration(slotIt, 0), slotIt < wbf.numSlots, IR_PreIncrement(slotIt), stmts.to[ListBuffer])

    if (wbf.numSlots > 1) wrapAroundSlotLoop(assign) else assign
  }
}

case class IR_WaLBerlaInitFieldInstances(onGPU : Boolean, wbFields : IR_WaLBerlaField*) extends IR_WaLBerlaFuturePlainFunction {
  override def name : String = s"initFieldInstances_${ wbFields.head.name }" + (if (onGPU) "_onGPU" else "")
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    var body = ListBuffer[IR_Statement]()

    // init pointers to waLBerla field datastructures in block loop via "getData" function
    for (wbf <- wbFields)
      body += IR_WaLBerlaInitFieldInstances.initRoutine(onGPU, wbf)

    body = ListBuffer(IR_WaLBerlaLoopOverLocalBlockArray(body))

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }
}
