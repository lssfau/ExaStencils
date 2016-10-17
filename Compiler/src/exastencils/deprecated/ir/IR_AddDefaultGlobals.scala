package exastencils.deprecated.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.util.ir._

@deprecated("to be split and moved to the corresponding packages", "13.10.16")
object IR_AddDefaultGlobals extends DefaultStrategy("AddDefaultGlobals") {
  this += new Transformation("Adding default global constants and variables", {
    case globals : IR_GlobalCollection => {
      if (Knowledge.cuda_enabled) {
        globals.variables += IR_VariableDeclaration("CUcontext", "cudaContext")
        globals.variables += IR_VariableDeclaration("CUdevice", "cudaDevice")
      }
      if (Knowledge.mpi_enabled) {
        // FIXME: introduce iv's
        globals.variables += IR_VariableDeclaration("MPI_Comm", "mpiCommunicator")
        globals.variables += IR_VariableDeclaration(IR_IntegerDatatype, "mpiSize")
      }
      globals
    }

    case func : IR_Function if "initGlobals" == func.name => {
      if (Knowledge.cuda_enabled) {
        // init device
        func.body ++= ListBuffer[IR_Statement](
          IR_VariableDeclaration(IR_IntegerDatatype, "deviceCount", 0),
          "cuDeviceGetCount(&deviceCount)",
          IR_Assert(IR_Lower(Knowledge.cuda_deviceId, "deviceCount"),
            ListBuffer("\"Invalid device id (\"", Knowledge.cuda_deviceId, "\") must be smaller than the number of devices (\"", "deviceCount", "\")\""),
            IR_FunctionCall("exit", 1)),
          s"cuDeviceGet(&cudaDevice, ${ Knowledge.cuda_deviceId })")

        // print device info (name)
        if (!Knowledge.l3tmp_genForAutoTests) {
          func.body ++= ListBuffer[IR_Statement](
            "cudaDeviceProp devProp",
            s"cudaGetDeviceProperties(&devProp, ${ Knowledge.cuda_deviceId })",
            IR_RawPrint("\"Using CUDA device \"", Knowledge.cuda_deviceId, "\": \"", "devProp.name", "std::endl"))
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
