package exastencils.waLBerla.ir.field

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks
import exastencils.waLBerla.ir.interfacing._
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

object IR_WaLBerlaInitFieldDataPtrs {
  def initRoutine(onGPU : Boolean, wbf : IR_WaLBerlaField) : IR_ForLoop = {
    val slotIt = IR_VariableAccess("slotIt", IR_IntegerDatatype)
    var defIt = IR_WaLBerlaLoopOverBlocks.defIt

    def getFieldData = IR_IV_WaLBerlaGetFieldData(wbf, slotIt, onGPU, defIt)

    def getField = IR_IV_WaLBerlaGetField(wbf, slotIt, onGPU, defIt)

    val initVal = if (wbf.layout.useFixedLayoutSizes) {
      // get ptr without offset -> referenceOffset handled by ExaStencils
      val ptr = new IR_MemberFunctionCallArrowWithDt(getField, "data", ListBuffer())
      if (onGPU) // TODO: offset for pitched pointers?
        IR_Cast(IR_PointerDatatype(getFieldData.baseDatatype()), ptr) // "data" function of GPU fields returns void*
      else
        ptr
    } else {
      // dataAt(0, 0, 0, 0) already points to first inner iteration point at "referenceOffset" -> referenceOffset not handled by ExaStencils
      if (wbf.layout.referenceOffset.forall(_ != IR_IntegerConstant(0)))
        Logger.error("IR_IV_WaLBerlaFieldDataAt assumes a referenceOffset of zero")

      // index handling for waLBerla accessors
      val index = Duplicate(wbf.layout.referenceOffset)
      val newIndex = IR_WaLBerlaUtil.adaptIndexForAccessors(index, wbf.gridDatatype, wbf.numDimsGrid, wbf.layout.numDimsData)

      // dataAt requires 4 arguments: x, y, z, f
      newIndex.indices = Duplicate(newIndex.indices).padTo(4, 0 : IR_Expression)

      // get field pointer at first inner iteration point at "referenceOffset"
      if (newIndex.length != 4)
        Logger.warn("waLBerla's \"dataAt\" function expects four arguments: x, y, z, f")

      new IR_MemberFunctionCallArrowWithDt(getField, "dataAt", newIndex.toExpressionIndex.indices.to[ListBuffer])
    }

    IR_ForLoop(IR_VariableDeclaration(slotIt, 0), slotIt < wbf.numSlots, IR_PreIncrement(slotIt),
      IR_Assignment(getFieldData, initVal))
  }
}

case class IR_WaLBerlaInitFieldDataPtrs(onGPU : Boolean, wbFields : IR_WaLBerlaField*) extends IR_WaLBerlaFuturePlainFunction {
  override def name : String = s"initFieldPointers_${ wbFields.head.name }" + (if (onGPU) "_onGPU" else "")
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    var body = ListBuffer[IR_Statement]()

    if (Knowledge.waLBerla_useInternalMemoryPointers) {
      // save pointers to internal memory pointers of waLBerla fields
      for (wbf <- wbFields)
        body += IR_WaLBerlaInitFieldDataPtrs.initRoutine(onGPU, wbf)

      body = ListBuffer(IR_WaLBerlaLoopOverBlocks(body, setupWaLBerlaFieldPointers = false))
    }

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }
}
