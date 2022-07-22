package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FunctionLike
import exastencils.base.ir.IR_Return
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest

object IR_WaLBerlaGetterFunctionCollection {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaGetBlockForest()
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
