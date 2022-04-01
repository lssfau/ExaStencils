package exastencils.parallelization.api.cuda

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.baseExt.ir.IR_LoopOverDomains
import exastencils.baseExt.ir.IR_LoopOverFields
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_LoopOverLevels
import exastencils.baseExt.ir.IR_LoopOverNeighbors
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_CommunicationKernelCollector
import exastencils.util.ir.IR_StackCollector

/// CUDA_Stream

object CUDA_Stream {
  def getStream(stackCollector: IR_StackCollector, communicateKernelCollector : IR_CommunicationKernelCollector) : CUDA_Stream = {
    val enclosingFragLoop = stackCollector.stack.collectFirst {
      case fragLoop : IR_LoopOverFragments                                                                                     => fragLoop
      case fragLoop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == IR_LoopOverFragments.defIt.name => fragLoop
    }

    val neighCommKernel = if (enclosingFragLoop.isDefined)
      communicateKernelCollector.getNeighbor(enclosingFragLoop.get)
    else
      None

    if (neighCommKernel.isDefined)
      CUDA_CommunicateStream(Duplicate(neighCommKernel.get))
    else
      CUDA_ComputeStream()
  }
}

abstract class CUDA_Stream(
    var perFragment : Boolean,
    var perNeighbor : Boolean,
) extends IR_InternalVariable(perFragment, false, false, false, perNeighbor) {

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

/// CUDA_ComputeStream

case class CUDA_ComputeStream(fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CUDA_Stream(true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  // use streams for fragment loops
  override def useNonDefaultStreams : Boolean = Knowledge.cuda_useStreams && Knowledge.domain_numFragmentsPerBlock > 1

  override def resolveName() = s"cudaComputeStream" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
}

/// CUDA_CommunicateStream

case class CUDA_CommunicateStream(neighborIdx : IR_Expression, fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CUDA_Stream(true, true) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighborIdx)

  // use streams for comm/boundary handling
  override def useNonDefaultStreams : Boolean = Knowledge.cuda_useStreams

  override def resolveName() = s"cudaCommStream" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", neighborIdx.prettyprint)
}