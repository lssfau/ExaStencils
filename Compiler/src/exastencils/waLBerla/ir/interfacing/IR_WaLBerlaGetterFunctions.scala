package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
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

  for (field <- IR_WaLBerlaFieldCollection.objects.groupBy(_.name).map(_._2.head)) {
    functions += IR_WaLBerlaGetBlockDataID(field)
  }
}

case class IR_WaLBerlaGetBlockForest() extends IR_WaLBerlaFuturePlainFunction {
  override def isInterfaceFunction : Boolean = true
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    val blockForest = IR_WaLBerlaBlockForest()
    IR_WaLBerlaPlainFunction(name, blockForest.datatype, ListBuffer(), ListBuffer(IR_Return(blockForest)))
  }
  override def name : String = "getBlockForest"
}

case class IR_WaLBerlaGetBlockDataID(field : IR_WaLBerlaField) extends IR_WaLBerlaFuturePlainFunction {
  override def isInterfaceFunction : Boolean = true
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    val lvl = IR_FunctionArgument("lvl", IR_IntegerDatatype)
    val slot = IR_FunctionArgument("slot", IR_IntegerDatatype)
    val onGPU = IR_FunctionArgument("onGPU", IR_BooleanDatatype)
    val blockDataId = IR_WaLBerlaBlockDataID(field, Duplicate(slot.access))
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
}
