package exastencils.parallelization.api.cuda

import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.prettyprinting.PpStream

/// CUDA_Init

case class CUDA_Init() extends CUDA_DeviceStatement {
  override def prettyprint(out : PpStream) : Unit = out << "cuInit(0);"
}

/// CUDA_Finalize

case class CUDA_Finalize() extends CUDA_DeviceStatement {
  override def prettyprint(out : PpStream) : Unit = {
    // has to be done after all other de-initialization statements
    out << "cuCtxDestroy(cudaContext);"
  }
}

/// CUDA_DeviceSynchronize

case class CUDA_DeviceSynchronize() extends CUDA_HostStatement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(IR_FunctionCall("cudaDeviceSynchronize"))
  }
}

/// CUDA_SyncThreads

case class CUDA_SyncThreads() extends CUDA_HostStatement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "__syncthreads();"
  }
}
