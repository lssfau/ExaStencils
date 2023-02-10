package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._

object IR_WaLBerlaDeInitFunctionCollection {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaDeDestroyBuffersWrapper()
}

// TODO: initGeometry

// wrappers for exa de-init functions

private case class IR_WaLBerlaDeDestroyBuffersWrapper() extends IR_WaLBerlaWrapperFunction {
  override def name : String = "cleanupExaBuffers"

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction =
    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(),
      ListBuffer[IR_Statement](IR_FunctionCall("destroyGlobals")))

  override def isInterfaceFunction : Boolean = true
  override def inlineImplementation : Boolean = true
}

