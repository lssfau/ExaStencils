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

      func.body.append(IR_Return(0))

      func
  })
}
