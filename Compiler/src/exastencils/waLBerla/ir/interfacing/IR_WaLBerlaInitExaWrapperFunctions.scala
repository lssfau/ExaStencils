package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._

// collection with setup functions for exastencils data structures
object IR_WaLBerlaInitExaWrapperFunctions {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaInitExaGlobalsWrapper()
}

// TODO: initGeometry?

// wrappers for exa init functions

private case class IR_WaLBerlaInitExaGlobalsWrapper() extends IR_WaLBerlaWrapperFunction {
  override def name : String = "initExaGlobals"

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction =
    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(),
      ListBuffer[IR_Statement](IR_FunctionCall("initGlobals")))

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true
}
