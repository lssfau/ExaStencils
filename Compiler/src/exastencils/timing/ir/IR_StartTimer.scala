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
import exastencils.parallelization.api.cuda.CUDA_DeviceSynchronize
import exastencils.parallelization.api.mpi.MPI_Barrier

/// IR_StartTimer

case class IR_StartTimer() extends IR_TimerFunction {

  import IR_TimerFunction._

  override var name = "startTimer"
  override def prettyprint_decl() : String = prettyprint

  override def generateFct() = {
    val statements = ListBuffer[IR_Statement](
      IR_IfCondition(IR_EqEq(0, accessMember("numEntries")), ListBuffer(
        if (Knowledge.timer_syncDevice && Knowledge.cuda_enabled) CUDA_DeviceSynchronize() else IR_NullStatement,
        if (Knowledge.timer_syncMpi && Knowledge.mpi_enabled) MPI_Barrier else IR_NullStatement,
        IR_AssignNowToTimer(accessMember("timerStarted")),
        IR_Assignment(accessMember("lastTimeMeasured"), IR_ZeroTimerValue()))),
      IR_PreIncrement(accessMember("numEntries")))

    val fct = IR_PlainFunction(name, IR_UnitDatatype, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), statements)
    fct.allowFortranInterface = false
    fct
  }
}
