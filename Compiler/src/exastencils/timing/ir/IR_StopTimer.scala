package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.prettyprinting.PpStream

/// IR_StopTimer

case class IR_StopTimer() extends IR_TimerFunction with IR_Expandable {

  import IR_TimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() : String = prettyprint
  override def name = "stopTimer"

  override def expand() : Output[IR_Function] = {
    val statements = ListBuffer[IR_Statement](
      IR_PreDecrementExpression(accessMember("numEntries")),
      IR_IfCondition(IR_EqEqExpression(0, accessMember("numEntries")), ListBuffer[IR_Statement](
        IR_AssignNowToTimer(accessMember("timerEnded")),
        IR_Assignment(accessMember("lastTimeMeasured"),
          if ("Chrono" == Knowledge.timer_type)
            IR_FunctionCall("std::chrono::duration_cast<std::chrono::nanoseconds>", accessMember("timerEnded") - accessMember("timerStarted"))
          else
            accessMember("timerEnded") - accessMember("timerStarted")),
        IR_Assignment(accessMember("totalTimeMeasured"), accessMember("lastTimeMeasured"), "+="),
        if (Knowledge.experimental_timerEnableCallStacks) "CallTracker::StopTimer(&stopWatch)" else "",
        IR_PreIncrementExpression(accessMember("numMeasurements")))))

    val fct = IR_Function(IR_UnitDatatype, name, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), statements)
    fct.allowFortranInterface = false
    fct
  }
}
