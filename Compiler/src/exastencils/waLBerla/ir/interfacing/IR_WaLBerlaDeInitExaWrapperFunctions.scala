package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._

object IR_WaLBerlaDeInitExaWrapperFunctions {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaDestroyExaBuffersWrapper()
}

// wrappers for exa de-init functions

private case class IR_WaLBerlaDestroyExaBuffersWrapper() extends IR_WaLBerlaWrapperFunction {
  override def name : String = "cleanupExaBuffers"

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction =
    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(),
      ListBuffer[IR_Statement](IR_FunctionCall("destroyGlobals")))

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true
}
