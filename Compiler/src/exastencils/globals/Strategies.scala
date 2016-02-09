package exastencils.globals

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.domain._
import exastencils.knowledge._
import exastencils.util._

object AddDefaultGlobals extends DefaultStrategy("AddDefaultGlobals") {
  this += new Transformation("Adding default global constants and variables", {
    case globals : Globals => {
      if (Knowledge.experimental_cuda_enabled) {
        globals.variables += new VariableDeclarationStatement("CUcontext", "cudaContext")
        globals.variables += new VariableDeclarationStatement("CUdevice", "cudaDevice")
      }
      if (Knowledge.mpi_enabled) {
        globals.variables += new VariableDeclarationStatement("MPI_Comm", "mpiCommunicator")
        globals.variables += new VariableDeclarationStatement(IntegerDatatype, "mpiRank")
        globals.variables += new VariableDeclarationStatement(IntegerDatatype, "mpiSize")
      }
      globals
    }

    case func : FunctionStatement if ("initGlobals" == func.name) => {
      if (Knowledge.experimental_cuda_enabled) {
        // init device
        func.body ++= ListBuffer[Statement](
          VariableDeclarationStatement(IntegerDatatype, "deviceCount", Some(0)),
          "cuDeviceGetCount(&deviceCount)",
          AssertStatement(LowerExpression(Knowledge.experimental_cuda_deviceId, "deviceCount"),
            ListBuffer("\"Invalid device id (\"", Knowledge.experimental_cuda_deviceId, "\") must be smaller than the number of devices (\"", "deviceCount", "\")\""),
            new FunctionCallExpression("exit", 1)),
          s"cuDeviceGet(&cudaDevice, ${Knowledge.experimental_cuda_deviceId})")

        // print device info (name)
        if (true) {
          func.body ++= ListBuffer[Statement](
            "cudaDeviceProp devProp",
            s"cudaGetDeviceProperties(&devProp, ${Knowledge.experimental_cuda_deviceId})",
            PrintStatement(ListBuffer("\"Using CUDA device \"", Knowledge.experimental_cuda_deviceId, "\": \"", "devProp.name", "std::endl")))
        }

        // create context
        func.body += "cuCtxCreate(&cudaContext, 0, cudaDevice)"
      }

      if (Knowledge.mpi_enabled) {
        func.body += "mpiCommunicator = " + Knowledge.mpi_defaultCommunicator
        func.body += "MPI_Comm_rank(mpiCommunicator, &mpiRank)"
        func.body += "MPI_Comm_size(mpiCommunicator, &mpiSize)"
        func.body += "std::srand(mpiRank)"
      }

      func
    }
  })
}
