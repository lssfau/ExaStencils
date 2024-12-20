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

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_StdVectorDatatype_VS
import exastencils.config._
import exastencils.core.StateManager
import exastencils.parallelization.api.mpi._
import exastencils.util.ir._

/// IR_PrintAllTimersToFile

case class IR_PrintAllTimersToFile() extends IR_TimerFunction with IR_TimerGatherer {
  override var name = "printAllTimersToFile"
  override def prettyprint_decl() : String = prettyprint

  def genPrint(timers : HashMap[(String, Option[Int]), IR_TimingIV]) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    def stride : IR_Expression = if (Knowledge.mpi_enabled && Knowledge.timer_printTimersToFileForEachRank) "mpiIt" else 0

    var it = 0
    val sep = "\"" + Settings.csvSeparatorEscaped() + "\""
    for (timer <- timers.toList.sortBy(_._1)) {
      val name = timer._2 match {
        case plainTimer : IR_PlainTimingIV          =>
          plainTimer.name
        case leveledTimer : IR_LeveledTimingIV =>
          leveledTimer.name + "_" + leveledTimer.level
      }
      statements += IR_Print(IR_VariableAccess("outFile", IR_UnknownDatatype), ListBuffer[IR_Expression](
        IR_StringConstant(name), sep,
        accessTimerValue((stride * (2 * timers.size)) + it), sep,
        accessTimerValue((stride * (2 * timers.size)) + it + 1), IR_StringConstant("\\n")))
      it += 2
    }

    // wrap in loop over each rank if required
    if (Knowledge.mpi_enabled && Knowledge.timer_printTimersToFileForEachRank) {
      statements = ListBuffer[IR_Statement](
        IR_ForLoop(
          IR_VariableDeclaration(IR_IntegerDatatype, stride.prettyprint, 0),
          IR_Lower(stride, MPI_IV_MpiSize),
          IR_PreIncrement(stride),
          statements))
    }

    statements.prepend(IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "open", "\"" + Settings.timerOutputFile + "\""))
    statements.prepend(IR_VariableDeclaration(IR_SpecialDatatype("std::ofstream"), "outFile"))
    statements.append(IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "close"))

    statements
  }

  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    var body : ListBuffer[IR_Statement] = ListBuffer()

    if (timers.nonEmpty) {
      if (Knowledge.timer_printTimersToFileForEachRank) {
        body += IR_IfCondition(MPI_IsRootProc(),
          ListBuffer[IR_Statement](
            createTimerValueStorage(MPI_IV_MpiSize * 2 * timers.size))
          ++ genDataCollect(timers, includeMeanTime = true)
          ++ ListBuffer[IR_Statement](MPI_Gather(timerValuesPointer, IR_DoubleDatatype, 2 * timers.size))
          ++ genPrint(timers),
          ListBuffer[IR_Statement](createTimerValueStorage(2 * timers.size))
            ++ genDataCollect(timers, includeMeanTime = true)
            ++ ListBuffer[IR_Statement](MPI_Gather(timerValuesPointer, timerValuesPointer, IR_DoubleDatatype, 2 * timers.size))
        )
      } else {
        body += createTimerValueStorage(2 * timers.size)
        body ++= genDataCollect(timers, includeMeanTime = true)
        body += MPI_Reduce(0, timerValuesPointer, IR_DoubleDatatype, 2 * timers.size, "+")
        if (Knowledge.mpi_enabled) {
          def timerId = IR_VariableAccess("timerId", IR_IntegerDatatype)

          body += IR_ForLoop(IR_VariableDeclaration(timerId, 0), IR_Lower(timerId, 2 * timers.size), IR_PreIncrement(timerId),
            IR_Assignment(accessTimerValue(timerId), MPI_IV_MpiSize, "/="))
        }
        body += IR_IfCondition(MPI_IsRootProc(), genPrint(timers))
      }
    }

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }
}
