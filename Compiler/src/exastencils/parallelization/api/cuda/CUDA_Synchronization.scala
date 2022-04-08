package exastencils.parallelization.api.cuda

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

/// CUDA_StreamSynchronize

case class CUDA_StreamSynchronize(stream : CUDA_Stream) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall("cudaStreamSynchronize", stream))
}

/// CUDA_DeviceSynchronize

case class CUDA_DeviceSynchronize() extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall("cudaDeviceSynchronize"))
}

/// CUDA_SyncThreads

case class CUDA_SyncThreads() extends CUDA_HostStatement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "__syncthreads();"
  }
}
