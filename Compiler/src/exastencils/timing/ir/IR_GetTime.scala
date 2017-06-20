package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._

/// IR_GetTotalTime

// in milliseconds
case class IR_GetTotalTime() extends IR_TimerFunction with IR_Expandable {

  import IR_TimerFunction._

  override def prettyprint_decl() : String = prettyprint
  override def name = "getTotalTime"

  override def generateFct() : IR_Function = {
    val body = IR_ReturnConvertToMS(accessMember("totalTimeMeasured"))
    val fct = IR_Function(IR_DoubleDatatype, name, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), body)
    fct.allowFortranInterface = false
    fct
  }
}

/// IR_GetMeanTime

// in milliseconds
case class IR_GetMeanTime() extends IR_TimerFunction with IR_Expandable {

  import IR_TimerFunction._

  override def prettyprint_decl() : String = prettyprint
  override def name = "getMeanTime"

  override def generateFct() : IR_Function = {
    val body = IR_Return(IR_TernaryCondition(
      IR_Greater(accessMember("numMeasurements"), 0),
      IR_FunctionCall("getTotalTime", "stopWatch") / accessMember("numMeasurements"),
      0.0))

    val fct = IR_Function(IR_DoubleDatatype, name, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), body)
    fct.allowFortranInterface = false
    fct
  }
}

/// IR_GetLastTime

// in milliseconds
case class IR_GetLastTime() extends IR_TimerFunction with IR_Expandable {

  import IR_TimerFunction._

  override def prettyprint_decl() : String = prettyprint
  override def name = "getLastTime"

  override def generateFct() : IR_Function = {
    val body = IR_ReturnConvertToMS(accessMember("lastTimeMeasured"))

    val fct = IR_Function(IR_DoubleDatatype, name, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), body)
    fct.allowFortranInterface = false
    fct
  }
}
