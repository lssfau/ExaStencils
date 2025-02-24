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
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_CommunicationKernelCollector
import exastencils.util.ir.IR_FragmentLoopCollector

/// CUDA_Stream

object CUDA_Stream {
  // get stream used for comm/comp kernels
  def getStream(fragLoopCollector: IR_FragmentLoopCollector, communicateKernelCollector : IR_CommunicationKernelCollector) : CUDA_Stream = {
    val enclosingFragLoop = fragLoopCollector.getEnclosingFragmentLoop()

    val neighCommKernel = if (enclosingFragLoop.isDefined)
      communicateKernelCollector.getNeighbor(enclosingFragLoop.get)
    else
      None

    if (neighCommKernel.isDefined)
      CUDA_CommunicateStream(Duplicate(neighCommKernel.get))
    else
      CUDA_ComputeStream()
  }

  def genCommSync() : ListBuffer[CUDA_StreamSynchronize] = DefaultNeighbors.neighbors.map(_.index).map(neigh =>
    CUDA_StreamSynchronize(CUDA_CommunicateStream(neigh)))

  def genCompSync() : CUDA_StreamSynchronize = CUDA_StreamSynchronize(CUDA_ComputeStream())

  def genSynchronize(stream : CUDA_Stream, before : Boolean) : ListBuffer[CUDA_StreamSynchronize] = {
    var stmts = ListBuffer[CUDA_StreamSynchronize]()

    // generate stream synchronization
    if (stream.useNonDefaultStreams) {
      // synchronize non-default streams
      val syncStream = CUDA_StreamSynchronize(stream)

      // before kernel
      val flag = stream match {
        case _ : CUDA_ComputeStream     =>
          if (before)
            Knowledge.experimental_cuda_syncStreamsBeforeComputeKernelCalls
          else
            Knowledge.experimental_cuda_syncStreamsAfterComputeKernelCalls
        case _ : CUDA_CommunicateStream =>
          if (before)
            Knowledge.experimental_cuda_syncStreamsBeforeCommunicateKernelCalls
          else
            Knowledge.experimental_cuda_syncStreamsAfterCommunicateKernelCalls
        case _                          =>
          Logger.error("Unknown CUDA stream instance passed to CUDA_StreamSynchronize")
      }

      val compSyncRefNeighIdx = if (before) 0 else DefaultNeighbors.neighbors.last.index // neighborIdx to consider for comp sync
      flag match {
        case "comp" | "all" => // computation
          stream match {
            case _ : CUDA_ComputeStream     =>
              stmts += syncStream
            case comm : CUDA_CommunicateStream if comm.neighborIdx == IR_IntegerConstant(compSyncRefNeighIdx) =>
              stmts += genCompSync()
            case _ =>
          }
        case "comm" | "all" => // communication
          stream match {
            case _ : CUDA_ComputeStream     =>
              stmts ++= genCommSync()
            case _ : CUDA_CommunicateStream =>
              stmts += syncStream
            case _ =>
          }
        case _              =>
      }
    }

    stmts
  }
}

abstract class CUDA_Stream(
    var perFragment : Boolean,
    var perField : Boolean,
    var perNeighbor : Boolean,
) extends IR_InternalVariable(perFragment, false, perField, false, perNeighbor) {

  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("cudaStream_t")

  def useNonDefaultStreams : Boolean

  override def getCtor() : Option[IR_Statement] = {
    if (useNonDefaultStreams) {
      Some(wrapInLoops(
        CUDA_CheckError(
          IR_FunctionCall(IR_ExternalFunctionReference("cudaStreamCreate"),
            IR_AddressOf(
              resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt))))))
    } else {
      None
    }
  }

  override def getDtor() : Option[IR_Statement] = {
    if (useNonDefaultStreams) {
      Some(wrapInLoops(
        CUDA_CheckError(
            IR_FunctionCall(IR_ExternalFunctionReference("cudaStreamDestroy"),
              resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)))))
    } else {
      None
    }
  }
}

/// CUDA_StreamMode

object CUDA_StreamMode {
  def streamToMode(stream : CUDA_Stream) = stream match {
    case _ : CUDA_ComputeStream     => CUDA_ComputeStreamMode()
    case _ : CUDA_CommunicateStream => CUDA_CommunicateStreamMode()
    case _                          => CUDA_DummyStreamMode()
  }

  // guard for equal stream modes
  def isCurrentStreamMode(stream : CUDA_Stream) : IR_Expression = streamToMode(stream) EqEq CUDA_CurrentStreamMode()
  def isCurrentStreamMode(mode : CUDA_StreamMode) : IR_Expression = mode EqEq CUDA_CurrentStreamMode()
}

abstract class CUDA_StreamMode(name : String) extends IR_UnduplicatedVariable {
  override def resolveName() : String = name
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype
}

// introduce IV to keep track over the current stream mode
case class CUDA_CurrentStreamMode() extends CUDA_StreamMode("currStreamMode") {
  override def resolveDefValue() : Option[IR_Expression] = CUDA_DummyStreamMode().resolveDefValue()
}

// IVs for enum-like representation
sealed case class CUDA_ComputeStreamMode() extends CUDA_StreamMode("compStreamMode") {
  override def resolveDefValue() : Option[IR_Expression] = Some(1)
}

sealed case class CUDA_CommunicateStreamMode() extends CUDA_StreamMode("commStreamMode") {
  override def resolveDefValue() : Option[IR_Expression] = Some(2)
}

sealed case class CUDA_DummyStreamMode() extends CUDA_StreamMode("dummyStreamMode") {
  override def resolveDefValue() : Option[IR_Expression] = Some(3)
}

/// CUDA_ComputeStream

case class CUDA_ComputeStream(fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CUDA_Stream(true, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  // use streams for fragment loops
  override def useNonDefaultStreams : Boolean = Knowledge.experimental_cuda_useStreams

  override def resolveName() = s"cudaComputeStream" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
}

/// CUDA_CommunicateStream

case class CUDA_CommunicateStream(neighborIdx : IR_Expression, fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CUDA_Stream(true, false,true) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighborIdx)

  // use streams for comm/boundary handling
  override def useNonDefaultStreams : Boolean = Knowledge.experimental_cuda_useStreams

  override def resolveName() = s"cudaCommStream" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", neighborIdx.prettyprint)
}

/// CUDA_TransferStream

case class CUDA_TransferStream(field: IR_FieldLike, fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CUDA_Stream(true, true, false) {

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx,
    if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  // use streams for asynchronous memory transfers
  override def useNonDefaultStreams : Boolean = Knowledge.experimental_cuda_useStreams

  override def resolveName() = s"transferStream" + resolvePostfix(fragmentIdx.prettyprint, "",
    if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, "", "")
}