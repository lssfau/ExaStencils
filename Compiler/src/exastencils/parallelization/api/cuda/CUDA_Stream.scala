package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.baseExt.ir.IR_LoopOverDomains
import exastencils.baseExt.ir.IR_LoopOverFields
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_LoopOverLevels
import exastencils.baseExt.ir.IR_LoopOverNeighbors
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_NeighborIsValid
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// CUDA_Stream

abstract class CUDA_Stream(
    var perFragment : Boolean,
    var perNeighbor : Boolean,
) extends IR_InternalVariable(perFragment, false, false, false, perNeighbor) {

  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("cudaStream_t")

  def useNonDefaultStreams = Knowledge.domain_numFragmentsPerBlock > 1

  override def getCtor() : Option[IR_Statement] = {
    if (useNonDefaultStreams) {
      Some(wrapInLoops(
        IR_FunctionCall(IR_ExternalFunctionReference("cudaStreamCreate"),
          IR_AddressOf(
            resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)))))
    } else {
      None
    }
  }

  override def getDtor() : Option[IR_Statement] = {
    if (useNonDefaultStreams) {
      Some(wrapInLoops(
        IR_FunctionCall(IR_ExternalFunctionReference("cudaStreamDestroy"),
          resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt))))
    } else {
      None
    }
  }
}

/// CUDA_ComputeStream

case class CUDA_ComputeStream(fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CUDA_Stream(true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"cudaComputeStream" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
}

/// CUDA_CommunicateStream

case class CUDA_CommunicateStream(neighborIdx : IR_Expression, fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CUDA_Stream(true, true) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighborIdx)

  override def resolveName() = s"cudaCommStream" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", neighborIdx.prettyprint)
}


/// CUDA_StreamSynchronize

object CUDA_StreamSynchronize {
  def genSynchronize(stream : CUDA_Stream, before : Boolean) : ListBuffer[IR_Statement] = {
    var stmts = ListBuffer[IR_Statement]()

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
        stmts += syncCommunication
      case _              =>
    }

    stmts
  }
}

case class CUDA_StreamSynchronize(stream : CUDA_Stream) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall("cudaStreamSynchronize", stream))
}