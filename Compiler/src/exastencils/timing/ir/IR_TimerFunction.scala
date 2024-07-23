//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.timing.l4.L4_TimerFunctions

/// IR_TimerFunction

object IR_TimerFunction {
  def accessMember(member : String) = IR_MemberAccess(IR_VariableAccess("stopWatch", IR_SpecialDatatype("StopWatch&")), member)
}

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

  val functions = L4_TimerFunctions.functions
}

case class IR_TimerFunctions() extends IR_FunctionCollection(IR_TimerFunctions.defBaseName,
  ListBuffer("fstream", "iostream", "vector"),  // todo check if it is legal to add vector here
  ListBuffer(IR_GlobalCollection.defHeader, IR_Stopwatch.defHeader)) {

  functions += IR_StartTimer()
  functions += IR_StopTimer()
  functions += IR_GetTotalTime()
  functions += IR_GetMeanTime()
  functions += IR_GetLastTime()
  if (Knowledge.timer_automaticTiming) {
    functions += IR_PrintAllTimersIncludingAutomatic()
  } else {
    functions += IR_PrintAllTimers()
  }
  functions += IR_PrintAllTimersToFile()
  functions += IR_ReduceTimers()
  if (Knowledge.mpi_enabled) {
    functions += IR_PrintTimerStatistics()
  }
}
