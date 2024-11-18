package exastencils.parallelization.api.cuda

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.OutputType
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// CUDA_Event

abstract class CUDA_Event(
    canBePerFragment : Boolean,
    canBePerDomain : Boolean,
    canBePerField : Boolean,
    canBePerLevel : Boolean,
    canBePerNeighbor : Boolean
) extends IR_InternalVariable(canBePerFragment, canBePerDomain, canBePerField, canBePerLevel, canBePerNeighbor) {

  def synchronizationNecessary = Knowledge.experimental_cuda_useStreams

  override def getCtor() : Option[IR_Statement] = {
    Some(wrapInLoops(
      CUDA_CheckError(
        IR_FunctionCall(IR_ExternalFunctionReference("cudaEventCreateWithFlags"),
          IR_AddressOf(
            resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)),
          IR_VariableAccess("cudaEventDisableTiming", IR_UnknownDatatype)))))
  }

  override def getDtor() : Option[IR_Statement] = {
    Some(wrapInLoops(
      CUDA_CheckError(
        IR_FunctionCall(IR_ExternalFunctionReference("cudaEventDestroy"),
          resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)))))
  }
}

case class CUDA_PendingStreamTransfers(
    var field : IR_FieldLike,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt)
  extends CUDA_Event(true, false, true, true, false) {

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression,
    if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, field.level, IR_NullExpression)

  override def resolveName() : String = "pendingStreamTransferEvent" +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, field.level.toString, "")

  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("cudaEvent_t")
}

case class CUDA_EventRecord(event : CUDA_Event, stream : CUDA_Stream) extends IR_Statement with IR_Expandable {
  override def expand() : OutputType = {
    if (stream.useNonDefaultStreams && event.synchronizationNecessary)
      CUDA_CheckError(IR_FunctionCall(IR_ExternalFunctionReference("cudaEventRecord"), event, stream))
    else
      IR_NullStatement
  }
}

case class CUDA_WaitEvent(event : CUDA_Event, stream : CUDA_Stream, direction : String) extends IR_Statement with IR_Expandable {

  def flags = 0

  override def expand() : OutputType = {
    if (stream.useNonDefaultStreams && event.synchronizationNecessary) {
      direction match {
        case "D2H" => CUDA_CheckError(IR_FunctionCall(IR_ExternalFunctionReference("cudaEventSynchronize"), event))
        case "H2D" => CUDA_CheckError(IR_FunctionCall(IR_ExternalFunctionReference("cudaStreamWaitEvent"), stream, event, flags))
        case _     => Logger.error("Unknown transfer direction: " + direction)
      }
    } else {
      IR_NullStatement
    }
  }
}