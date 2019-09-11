package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._

/// IR_StartTimer

case class IR_StartTimer() extends IR_TimerFunction {

  import IR_TimerFunction._

  override var name = "startTimer"
  override def prettyprint_decl() : String = prettyprint

  override def generateFct() = {
    val statements = ListBuffer[IR_Statement](
      IR_IfCondition(IR_EqEq(0, accessMember("numEntries")), ListBuffer(
        IR_AssignNowToTimer(accessMember("timerStarted")),
        IR_Assignment(accessMember("lastTimeMeasured"), IR_ZeroTimerValue()))),
      IR_PreIncrement(accessMember("numEntries")))

    val fct = IR_PlainFunction(name, IR_UnitDatatype, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), statements)
    fct.allowFortranInterface = false
    fct
  }
}
