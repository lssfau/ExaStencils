package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_NeighborIsValid
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// CUDA_Synchronize

object CUDA_Synchronize {
  def genSynchronize(stream : CUDA_Stream, before : Boolean) : ListBuffer[IR_Statement] = {
    var stmts = ListBuffer[IR_Statement]()

    // generate synchronization
    if (stream.useNonDefaultStreams) {
      // synchronize non-default streams
      val syncCommunication = DefaultNeighbors.neighbors.map(_.index).map(neigh =>
        IR_IfCondition(IR_IV_NeighborIsValid(0, neigh), CUDA_StreamSynchronize(CUDA_CommunicateStream(neigh))))

      val syncComputation = CUDA_StreamSynchronize(CUDA_ComputeStream())

      // before kernel
      val flag = stream match {
        case _ : CUDA_ComputeStream     =>
          if (before)
            Knowledge.cuda_syncStreamsBeforeComputeKernelCalls
          else
            Knowledge.cuda_syncStreamsAfterComputeKernelCalls
        case _ : CUDA_CommunicateStream =>
          if (before)
            Knowledge.cuda_syncStreamsBeforeCommunicateKernelCalls
          else
            Knowledge.cuda_syncStreamsAfterCommunicateKernelCalls
        case _                          =>
          Logger.error("Unknown CUDA stream instance passed to CUDA_StreamSynchronize")
      }

      flag match {
        case "comp" | "all" => // computation
          stmts += syncComputation
        case "comm" | "all" => // communication
          stmts ++= syncCommunication
        case _              =>
      }
    } else if (Knowledge.cuda_syncDeviceAfterKernelCalls) {
      // no streams employed -> sync device
      stmts += CUDA_DeviceSynchronize()
    }

    stmts
  }
}

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
