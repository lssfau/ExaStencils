package exastencils.globals

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.util._

object AddDefaultGlobals extends DefaultStrategy("AddDefaultGlobals") {
  this += new Transformation("Adding default global constants and variables", {
    case globals : Globals => {
      if (Knowledge.cuda_enabled) {
        globals.variables += new VariableDeclarationStatement("CUcontext", "cudaContext")
        globals.variables += new VariableDeclarationStatement("CUdevice", "cudaDevice")
      }
      if (Knowledge.mpi_enabled) {
        globals.variables += new VariableDeclarationStatement("MPI_Comm", "mpiCommunicator")
        globals.variables += new VariableDeclarationStatement(IR_IntegerDatatype, "mpiRank")
        globals.variables += new VariableDeclarationStatement(IR_IntegerDatatype, "mpiSize")
      }
      globals
    }

    case func : IR_Function if ("initGlobals" == func.name) => {
      if (Knowledge.cuda_enabled) {
        // init device
        func.body ++= ListBuffer[IR_Statement](
          VariableDeclarationStatement(IR_IntegerDatatype, "deviceCount", Some(0)),
          "cuDeviceGetCount(&deviceCount)",
          AssertStatement(IR_LowerExpression(Knowledge.cuda_deviceId, "deviceCount"),
            ListBuffer("\"Invalid device id (\"", Knowledge.cuda_deviceId, "\") must be smaller than the number of devices (\"", "deviceCount", "\")\""),
            new FunctionCallExpression("exit", 1)),
          s"cuDeviceGet(&cudaDevice, ${ Knowledge.cuda_deviceId })")

        // print device info (name)
        if (!Knowledge.l3tmp_genForAutoTests) {
          func.body ++= ListBuffer[IR_Statement](
            "cudaDeviceProp devProp",
            s"cudaGetDeviceProperties(&devProp, ${ Knowledge.cuda_deviceId })",
            PrintStatement(ListBuffer("\"Using CUDA device \"", Knowledge.cuda_deviceId, "\": \"", "devProp.name", "std::endl")))
        }

        // create context
        func.body += "cuCtxCreate(&cudaContext, 0, cudaDevice)"

        // set L1 cache and shared memory configuration for this device
        if (Knowledge.cuda_useSharedMemory)
          func.body += "cudaDeviceSetCacheConfig(cudaFuncCachePreferShared)"
        if (Knowledge.cuda_favorL1CacheOverSharedMemory)
          func.body += "cudaDeviceSetCacheConfig(cudaFuncCachePreferL1)"
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
