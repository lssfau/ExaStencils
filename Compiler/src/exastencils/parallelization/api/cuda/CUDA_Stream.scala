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
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_NeighborIsValid
import exastencils.field.ir.IR_Field
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_CommunicationKernelCollector
import exastencils.util.ir.IR_StackCollector

/// CUDA_Stream

object CUDA_Stream {
  // get stream used for comm/comp kernels
  def getStream(stackCollector: IR_StackCollector, communicateKernelCollector : IR_CommunicationKernelCollector) : CUDA_Stream = {
    val enclosingFragLoop = stackCollector.stack.collectFirst {
      case fragLoop : IR_LoopOverFragments                                                                                     => fragLoop
      case fragLoop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == IR_LoopOverFragments.defIt.name => fragLoop
    }

    val neighCommKernel = if (enclosingFragLoop.isDefined)
      communicateKernelCollector.getNeighbor(enclosingFragLoop.get)
    else
      None

    val stream = if (neighCommKernel.isDefined)
      CUDA_CommunicateStream(Duplicate(neighCommKernel.get))
    else
      CUDA_ComputeStream()

    if (!stream.useNonDefaultStreams)
      CUDA_DefaultStream()
    else
      stream
  }

  def genSynchronize(stream : CUDA_Stream, before : Boolean) : ListBuffer[IR_Statement] = {
    var stmts = ListBuffer[IR_Statement]()

    // generate stream synchronization
    if (stream.useNonDefaultStreams) {
      // synchronize non-default streams
      val syncStream = CUDA_StreamSynchronize(stream)

      val syncAllCommunication = DefaultNeighbors.neighbors.map(_.index).map(neigh =>
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

      val compSyncRefNeighIdx = if (before) 0 else DefaultNeighbors.neighbors.last.index // neighborIdx to consider for comp sync
      flag match {
        case "comp" | "all" => // computation
          stream match {
            case _ : CUDA_ComputeStream     =>
              stmts += syncStream
            case comm : CUDA_CommunicateStream if comm.neighborIdx == IR_IntegerConstant(compSyncRefNeighIdx) =>
              stmts += syncComputation
            case _ =>
          }
        case "comm" | "all" => // communication
          stream match {
            case _ : CUDA_ComputeStream     =>
              stmts ++= syncAllCommunication
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

/// CUDA_DefaultStream

case class CUDA_DefaultStream() extends CUDA_Stream(false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def useNonDefaultStreams : Boolean = false

  override def resolveName() : String = "defaultStream"

  override def getCtor() : Option[IR_Statement] = None
  override def getDtor() : Option[IR_Statement] = None
}

/// CUDA_ComputeStream

case class CUDA_ComputeStream(fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CUDA_Stream(true, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  // use streams for fragment loops
  override def useNonDefaultStreams : Boolean = Knowledge.cuda_useStreams && Knowledge.domain_numFragmentsPerBlock > 1

  override def resolveName() = s"cudaComputeStream" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
}

/// CUDA_CommunicateStream

case class CUDA_CommunicateStream(neighborIdx : IR_Expression, fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CUDA_Stream(true, false,true) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighborIdx)

  // use streams for comm/boundary handling
  override def useNonDefaultStreams : Boolean = Knowledge.cuda_useStreams

  override def resolveName() = s"cudaCommStream" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", neighborIdx.prettyprint)
}

/// CUDA_TransferStream

case class CUDA_TransferStream(field: IR_Field, fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CUDA_Stream(true, true, false) {

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx,
    if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  // use streams for asynchronous memory transfers
  override def useNonDefaultStreams : Boolean = Knowledge.cuda_useStreams

  override def resolveName() = s"transferStream" + resolvePostfix(fragmentIdx.prettyprint, "",
    if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, "", "")
}