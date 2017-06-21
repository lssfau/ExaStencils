package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config.Knowledge

/// IR_TimerFunction

object IR_TimerFunction {
  def accessMember(member : String) = IR_MemberAccess(IR_VariableAccess("stopWatch", IR_SpecialDatatype("StopWatch&")), member)
}

// TODO: leveled timer functions?
trait IR_TimerFunction extends IR_FutureFunction

/// IR_TimerFunctions

case class IR_TimerFunctions() extends IR_FunctionCollection("Util/TimerFunctions",
  ListBuffer("fstream", "iostream"),
  ListBuffer("Globals/Globals.h", "Util/Stopwatch.h")) {

  functions += IR_StartTimer()
  functions += IR_StopTimer()
  functions += IR_GetTotalTime()
  functions += IR_GetMeanTime()
  functions += IR_GetLastTime()
  functions += IR_PrintAllTimers()
  functions += IR_PrintAllTimersToFile()

  if (Knowledge.experimental_timerEnableCallStacks) {
    internalDependencies += s"Util/CallEntity.h"
    internalDependencies += s"Util/CallTracker.h"
  }
}
