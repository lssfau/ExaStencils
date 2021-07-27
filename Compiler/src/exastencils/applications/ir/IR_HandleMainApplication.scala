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
import exastencils.base.ir.{ IR_Native, _ }
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.parallelization.api.cuda._
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.api.omp.OMP_Parallel
import exastencils.visualization.ir.IR_SetupVisit

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

      if ("likwid" == Knowledge.benchmark_backend) {
        func.body.prepend(OMP_Parallel(ListBuffer(IR_Native("LIKWID_MARKER_THREADINIT"))))
        func.body.prepend(IR_Native("LIKWID_MARKER_INIT"))
        func.body.append(IR_Native("LIKWID_MARKER_CLOSE"))
      }

      if (Knowledge.cuda_enabled) {
        func.body.prepend(CUDA_Init)
        func.body.append(CUDA_Finalize)
      }

      if (Knowledge.mpi_enabled) {
        func.body.prepend(MPI_Init)
        func.body.append(MPI_Finalize)
      }

      if (Knowledge.experimental_visit_enable) {
        func.body.prepend(IR_SetupVisit.setupFct_visit_init())
        func.body.append(IR_SetupVisit.setupFct_visit_destroy())
      }

      func.body.append(IR_Return(0))

      func
  })
}
