package exastencils.visualization.ir.interactive.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._

/// IR_VisItControlCommandCallback
// implement functionality for the control buttons on the GUI

case class IR_VisItControlCommandCallback() extends IR_VisItFuturePlainFunction {

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()

    val cmd = IR_VariableAccess("cmd", IR_SpecialDatatype("const char*"))

    fctBody += IR_VisItCommandHandling(cmd)

    IR_PlainFunction(
      name,
      IR_UnitDatatype,
      ListBuffer(IR_FunctionArgument(cmd), IR_FunctionArgument("args", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }

  override def name : String = "ControlCommandCallback"
}
