package exastencils.parallelization.api.cuda

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.OutputType
import exastencils.field.ir.IR_Field

/// CUDA_Event

abstract class CUDA_Event(
    canBePerFragment : Boolean,
    canBePerDomain : Boolean,
    canBePerField : Boolean,
    canBePerLevel : Boolean,
    canBePerNeighbor : Boolean
) extends IR_InternalVariable(canBePerFragment, canBePerDomain, canBePerField, canBePerLevel, canBePerNeighbor) {

  override def getCtor() : Option[IR_Statement] = {
    Some(wrapInLoops(
      IR_FunctionCall(IR_ExternalFunctionReference("cudaEventCreateWithFlags"),
        IR_AddressOf(
          resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)),
        IR_VariableAccess("cudaEventDisableTiming", IR_UnknownDatatype))))
  }

  override def getDtor() : Option[IR_Statement] = {
    Some(wrapInLoops(
      IR_FunctionCall(IR_ExternalFunctionReference("cudaEventDestroy"),
        resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt))))
  }
}

case class CUDA_PendingStreamTransfers(
    var field : IR_Field,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt)
  extends CUDA_Event(true, false, true, false, false) {

  override def resolveName() : String = "pendingStreamTransferEvent" +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, "", "")

  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("cudaEvent_t")
}

case class CUDA_EventRecord(event : CUDA_Event, stream : CUDA_Stream) extends IR_Statement with IR_Expandable {
  override def expand() : OutputType = {
    if (stream.useNonDefaultStreams)
      IR_FunctionCall(IR_ExternalFunctionReference("cudaEventRecord"), IR_AddressOf(event), stream)
    else
      IR_NullStatement
  }
}

case class CUDA_WaitEvent(event : CUDA_Event, stream : CUDA_Stream) extends IR_Statement with IR_Expandable {
  override def expand() : OutputType = {
    if (stream.useNonDefaultStreams)
      IR_FunctionCall(IR_ExternalFunctionReference("cudaStreamWaitEvent"), stream, event)
    else
      IR_NullStatement
  }
}