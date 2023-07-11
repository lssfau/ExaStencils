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
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.parallelization.api.mpi.MPI_AllReduce

/// IR_ReduceTimers

case class IR_ReduceTimers() extends IR_TimerFunction {
  override var name = "reduceTimers"
  override def prettyprint_decl() : String = prettyprint

  def genReduceTimerCode(timer : IR_TimingIV) : IR_Statement = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += IR_Assignment(IR_MemberAccess(timer, "totalTimeAveraged"), IR_FunctionCall(IR_TimerFunctionReference("getTotalTime", IR_DoubleDatatype), timer))

    if (Knowledge.mpi_enabled) {
      val timerValue = IR_MemberAccess(timer, "totalTimeAveraged")
      statements += MPI_AllReduce(IR_AddressOf(timerValue), IR_DoubleDatatype, 1, "+")
      statements += IR_Assignment(timerValue, timerValue / Knowledge.mpi_numThreads)
    }

    IR_Scope(statements)
  }

  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    val body = timers.toList.sortBy(_._1).map(t => genReduceTimerCode(t._2)).to[ListBuffer]

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }
}
