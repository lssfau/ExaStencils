package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.core._
import exastencils.globals.ir.IR_GlobalCollection

/// IR_TimerFunction

object IR_TimerFunction {
  def accessMember(member : String) = IR_MemberAccess(IR_VariableAccess("stopWatch", IR_SpecialDatatype("StopWatch&")), member)
}

// TODO: leveled timer functions?
trait IR_TimerFunction extends IR_FutureFunction

/// IR_TimerFunctions

object IR_TimerFunctions extends ObjectWithState {
  def defBaseName = "Util/Timer"
  def defHeader = defBaseName + ".h"

  // buffer looked up reference to reduce execution time
  var selfRef : Option[IR_GlobalCollection] = None

  override def clear() = { selfRef = None }

  // looks itself up starting from the current root
  def get = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[IR_GlobalCollection]()
    selfRef.get
  }
}

case class IR_TimerFunctions() extends IR_FunctionCollection(IR_TimerFunctions.defBaseName,
  ListBuffer("fstream", "iostream"),
  ListBuffer(IR_GlobalCollection.defHeader, IR_Stopwatch.defHeader)) {

  functions += IR_StartTimer()
  functions += IR_StopTimer()
  functions += IR_GetTotalTime()
  functions += IR_GetMeanTime()
  functions += IR_GetLastTime()
  functions += IR_PrintAllTimers()
  functions += IR_PrintAllTimersToFile()
}
