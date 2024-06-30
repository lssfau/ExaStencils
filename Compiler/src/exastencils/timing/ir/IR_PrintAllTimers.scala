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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverLevels
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.parallelization.api.mpi.MPI_AllReduce
import exastencils.parallelization.api.mpi.MPI_IV_MpiSize
import exastencils.util.ir.IR_RawPrint

/// IR_PrintAllTimers

case class IR_PrintAllTimers() extends IR_TimerFunction {
  override var name = "printAllTimers"
  override def prettyprint_decl() : String = prettyprint

  def genPrintTimerCode(timer : IR_TimingIV) : IR_Statement = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val timeToPrint = "getTotalTime"

    def timerValue = IR_VariableAccess("timerValue", IR_DoubleDatatype)

    timer match {
      case IR_IV_Timer(_)                              => // non-leveled timer
        statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(IR_TimerFunctionReference(timeToPrint, IR_DoubleDatatype, None), timer.resolveName()))

        if (Knowledge.mpi_enabled) {
          statements += MPI_AllReduce(IR_AddressOf(timerValue), timerValue.datatype, 1, "+")
          statements += IR_Assignment(timerValue, MPI_IV_MpiSize, "/=")
        }

        statements += IR_RawPrint("\"Mean mean total time for Timer " + timer.name + ":\"", "timerValue")
      case leveledTimer @ IR_IV_LeveledTimer(_, level) =>
        val loopStatements : ListBuffer[IR_Statement] = ListBuffer()

        loopStatements += IR_VariableDeclaration(timerValue, IR_FunctionCall(
          IR_TimerFunctionReference(timeToPrint, IR_DoubleDatatype, Option(level)), leveledTimer.accessTimerAtIndex()
        ))
        // todo there must be a better way to do this
        loopStatements += IR_RawPrint("\"Mean mean total time for Timer " + timer.name + " at level\"", IR_LoopOverLevels.defIt, "\":\"", "timerValue")

        statements += timer.wrapInLoops(
          IR_Scope(
            loopStatements
          )
        )
    }
    IR_Scope(statements)
  }


  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    val body = timers.toList.sortBy(_._1).map(t => genPrintTimerCode(t._2)).to[ListBuffer]

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }
}
