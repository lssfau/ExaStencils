package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnitDatatype

case class IR_WaLBerlaDeInitInterfaceVariables(
    var body : ListBuffer[IR_Statement]
) extends IR_WaLBerlaWrapperFunction {

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction =
    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = false
  override def name : String = "cleanupInterfaceVariables"
}

