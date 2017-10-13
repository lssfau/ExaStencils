package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge

/// IR_StopTimer

case class IR_StopTimer() extends IR_TimerFunction {

  import IR_TimerFunction._

  override var name = "stopTimer"
  override def prettyprint_decl() : String = prettyprint

  override def generateFct() = {
    val statements = ListBuffer[IR_Statement](
      IR_PreDecrement(accessMember("numEntries")),
      IR_IfCondition(IR_EqEq(0, accessMember("numEntries")), ListBuffer[IR_Statement](
        IR_AssignNowToTimer(accessMember("timerEnded")),
        IR_Assignment(accessMember("lastTimeMeasured"),
          if ("Chrono" == Knowledge.timer_type)
            IR_FunctionCall("std::chrono::duration_cast<std::chrono::nanoseconds>", accessMember("timerEnded") - accessMember("timerStarted"))
          else
            accessMember("timerEnded") - accessMember("timerStarted")),
        IR_Assignment(accessMember("totalTimeMeasured"), accessMember("lastTimeMeasured"), "+="),
        if (Knowledge.experimental_timerEnableCallStacks) "CallTracker::StopTimer(&stopWatch)" else "",
        IR_PreIncrement(accessMember("numMeasurements")))))

    val fct = IR_PlainFunction(name, IR_UnitDatatype, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), statements)
    fct.allowFortranInterface = false
    fct
  }
}
