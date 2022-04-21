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

package exastencils.applications.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Native
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.api.omp.OMP_Parallel
import exastencils.timing.ir.IR_CollectUnresolvedTimers

/// IR_HandleMainApplication

object IR_HandleMainApplication extends DefaultStrategy("HandleMainApplication") {
  this += new Transformation("ResolveFunctionCalls", {
    case func : IR_Function if "Application" == func.name =>
      func.datatype = IR_IntegerDatatype
      func.name = "main"

      if (func.parameters.nonEmpty)
        Logger.warning("function Application is not allowed to have parameters, omitting them")
      func.parameters = ListBuffer(IR_FunctionArgument("argc", IR_IntegerDatatype), IR_FunctionArgument("argv", IR_SpecialDatatype("char**")))

      func.allowFortranInterface = false
      func.allowInlining = false

      def wrapAroundParallelRegion(body : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
        if (Knowledge.omp_enabled)
          ListBuffer(OMP_Parallel(body))
        else
          body
      }

      if ("likwid" == Knowledge.benchmark_backend) {
        // register timers
        if (Knowledge.timer_addBenchmarkMarkers) {
          var registerMarkers = ListBuffer[IR_Statement]()

          IR_CollectUnresolvedTimers.applyStandalone(StateManager.root)
          IR_CollectUnresolvedTimers.timers foreach { name =>
            registerMarkers += IR_Native("LIKWID_MARKER_REGISTER(\"" + name + "\")")
          }

          func.body.prependAll(wrapAroundParallelRegion(registerMarkers))
        }

        func.body.prependAll(wrapAroundParallelRegion(ListBuffer[IR_Statement](IR_Native("LIKWID_MARKER_THREADINIT"))))
        func.body.prepend(IR_Native("LIKWID_MARKER_INIT"))
        func.body.append(IR_Native("LIKWID_MARKER_CLOSE"))
      }

      if (Knowledge.mpi_enabled) {
        func.body.prepend(MPI_Init)
        func.body.append(MPI_Finalize)
      }

      func.body.append(IR_Return(0))

      func
  })
}
