package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_BlockDataID

object IR_WaLBerlaGetterFunctionCollection {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaGetBlockForest()
  functions += IR_WaLBerlaGetMaxLevel()
  functions += IR_WaLBerlaGetMinLevel()
  functions += IR_WaLBerlaGetNumLevels()

  for (field <- IR_WaLBerlaFieldCollection.objects.groupBy(_.name).map(_._2.head)) {
    functions += IR_WaLBerlaGetBlockDataID(field)
  }
}

case class IR_WaLBerlaGetBlockForest() extends IR_WaLBerlaWrapperFunction {
  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    val blockForest = IR_WaLBerlaBlockForest()
    IR_WaLBerlaPlainFunction(name, blockForest.datatype, ListBuffer(), ListBuffer(IR_Return(blockForest)))
  }
  override def name : String = "getBlockForest"

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true
}

case class IR_WaLBerlaGetMaxLevel() extends IR_WaLBerlaWrapperFunction {
  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    IR_WaLBerlaPlainFunction(name, IR_IntegerDatatype, ListBuffer(), ListBuffer(IR_Return(Knowledge.maxLevel)))
  }
  override def name : String = "getMaxMGLevel"

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true
}

case class IR_WaLBerlaGetMinLevel() extends IR_WaLBerlaWrapperFunction {
  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    IR_WaLBerlaPlainFunction(name, IR_IntegerDatatype, ListBuffer(), ListBuffer(IR_Return(Knowledge.minLevel)))
  }
  override def name : String = "getMinMGLevel"

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true
}

case class IR_WaLBerlaGetNumLevels() extends IR_WaLBerlaWrapperFunction {
  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    IR_WaLBerlaPlainFunction(name, IR_IntegerDatatype, ListBuffer(), ListBuffer(IR_Return(Knowledge.maxLevel - Knowledge.minLevel + 1)))
  }
  override def name : String = "getNumberOfMGLevels"

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true
}

case class IR_WaLBerlaGetBlockDataID(field : IR_WaLBerlaField) extends IR_WaLBerlaWrapperFunction {
  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    val lvl = IR_FunctionArgument("lvl", IR_IntegerDatatype)
    val slot = IR_FunctionArgument("slot", IR_IntegerDatatype)
    val onGPU = IR_FunctionArgument("onGPU", IR_BooleanDatatype)
    val blockDataId = IR_WaLBerlaBlockDataID(field, Duplicate(slot.access), onGPU = false)
    val blockDataIdGPU = IR_WaLBerlaBlockDataID(field, Duplicate(slot.access), onGPU = true)
    blockDataId.level = Duplicate(lvl.access)

    val body : ListBuffer[IR_Statement] = if (Knowledge.cuda_enabled) {
      ListBuffer(
        IR_IfCondition(onGPU.access,
          IR_Return(blockDataIdGPU),
          IR_Return(blockDataId)))
    } else {
      ListBuffer(IR_Return(blockDataId))
    }

    IR_WaLBerlaPlainFunction(name, WB_BlockDataID, ListBuffer(lvl, slot, onGPU), body)
  }

  override def name : String = s"getBlockDataID_${field.name}"
  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true
}
