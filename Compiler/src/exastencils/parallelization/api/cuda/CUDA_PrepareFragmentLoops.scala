package exastencils.parallelization.api.cuda

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_IV_CommBufferLike
import exastencils.core.Duplicate
import exastencils.fieldlike.ir.IR_IV_AbstractFieldLikeData
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.util.ir.IR_CommunicationKernelCollector
import exastencils.util.ir.IR_FragmentLoopCollector

trait CUDA_PrepareFragmentLoops extends CUDA_PrepareBufferSync with CUDA_ExecutionBranching {

  def fieldAccesses : mutable.Map[String, IR_IV_AbstractFieldLikeData]
  def bufferAccesses : mutable.Map[String, IR_IV_CommBufferLike]

  def accessedElementsFragLoop : mutable.HashMap[IR_ScopedStatement with IR_HasParallelizationInfo, CUDA_AccessedElementsInFragmentLoop]

  def createFragLoopHandler(loop : IR_ScopedStatement with IR_HasParallelizationInfo) = {
    if (accessedElementsFragLoop.contains(loop)) {
      val accessedElements = accessedElementsFragLoop(loop)
      CUDA_HandleFragmentLoops(loop, Duplicate(accessedElements))
    } else {
      loop
    }
  }

  // every LoopOverDimensions statement is potentially worse to transform in CUDA code
  // Exceptions:
  // 1. this loop is a special one and cannot be optimized in polyhedral model
  // 2. this loop has no parallel potential
  // use the host for dealing with the two exceptional cases
  def isLoopParallel(loop : IR_LoopOverDimensions) = {
    loop.parallelization.potentiallyParallel // filter some generate loops?
  }

  def findContainedLoopOverDims(cl : IR_ContractingLoop) = cl.body.find(s =>
    s.isInstanceOf[IR_IfCondition] || s.isInstanceOf[IR_LoopOverDimensions]) match {
    case Some(IR_IfCondition(cond, trueBody : ListBuffer[IR_Statement], ListBuffer())) =>
      val bodyWithoutComments = trueBody.filterNot(x => x.isInstanceOf[IR_Comment])
      bodyWithoutComments match {
        case ListBuffer(loop : IR_LoopOverDimensions) => loop
        case _                                        => IR_LoopOverDimensions(0, IR_ExpressionIndexRange(IR_ExpressionIndex(), IR_ExpressionIndex()), ListBuffer[IR_Statement]())
      }
    case Some(loop : IR_LoopOverDimensions)                                            =>
      loop
    case None                                                                          => IR_LoopOverDimensions(0, IR_ExpressionIndexRange(IR_ExpressionIndex(), IR_ExpressionIndex()), ListBuffer[IR_Statement]())
  }

  def collectAccessedBuffers(stmts : IR_Statement*) : Unit = {}

  def collectAccessedElementsFragmentLoop(
      body : ListBuffer[IR_Statement],
      fragLoopCollector : IR_FragmentLoopCollector,
      commKernelCollector : IR_CommunicationKernelCollector,
      isParallel : Boolean,
      fromMPIStatement : Boolean,
      estimatedHostTime : Double,
      estimatedDeviceTime : Double) = {

    // collect elements accessed in enclosing fragment loop and create handler
    val enclosingFragLoop = fragLoopCollector.getEnclosingFragmentLoop()
    if (enclosingFragLoop.isDefined) {
      val emptyElements = CUDA_AccessedElementsInFragmentLoop(ListBuffer(), mutable.HashMap(), mutable.HashMap(),
        isLoopParallel = true, fromMPIStatement = false, 0.0, 0.0)
      val elements = accessedElementsFragLoop.getOrElse(enclosingFragLoop.get, emptyElements)

      // get accessed buffers
      collectAccessedBuffers(body : _*)

      // determine execution ( = comm/comp ) stream
      val executionStream = CUDA_Stream.getStream(fragLoopCollector, commKernelCollector)

      // add accessed streams
      if (!elements.streams.contains(executionStream))
        elements.streams += executionStream

      // add accessed buffers/fields
      elements.fieldAccesses ++= fieldAccesses.map { case (str, fAcc) => str -> IR_IV_AbstractFieldLikeData(Duplicate(fAcc.field), Duplicate(fAcc.slot), Duplicate(fAcc.fragmentIdx)) }
      elements.bufferAccesses ++= bufferAccesses.map(Duplicate(_))

      // check if loop is parallel
      elements.isLoopParallel &= isParallel

      // check if accessed elements origin from mpi statement
      elements.fromMPIStatement = fromMPIStatement

      // add timings
      elements.estimatedHostTime = estimatedHostTime
      elements.estimatedDeviceTime = estimatedDeviceTime

      accessedElementsFragLoop.update(enclosingFragLoop.get, elements)
    }
  }

  def syncEventsBeforeHost(stream : CUDA_Stream) = {
    var beforeHost : ListBuffer[IR_Statement] = ListBuffer()

    // wait for pending transfer events if dirty flag is set and then reset flag
    for (access <- fieldAccesses.toSeq.sortBy(_._1)) {
      val fieldData = access._2
      if (syncBeforeHost(access._1, fieldAccesses.keys)) {
        val dirtyFlag = CUDA_DeviceDataUpdated(fieldData.field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx))
        beforeHost += IR_IfCondition(dirtyFlag EqEq CUDA_DirtyFlagCase.INTERMEDIATE.id,
          ListBuffer[IR_Statement](
            CUDA_WaitEvent(CUDA_PendingStreamTransfers(fieldData.field, Duplicate(fieldData.fragmentIdx)), stream, "D2H"),
            IR_Assignment(dirtyFlag, CUDA_DirtyFlagCase.CLEAR.id)))
      }
    }
    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      val buffer = access._2
      if (syncBeforeHost(access._1, bufferAccesses.keys)) {
        val dirtyFlag = CUDA_DeviceBufferDataUpdated(buffer.field, buffer.send, Duplicate(buffer.neighIdx))
        beforeHost += IR_IfCondition(dirtyFlag EqEq CUDA_DirtyFlagCase.INTERMEDIATE.id,
          ListBuffer[IR_Statement](
            CUDA_WaitEvent(CUDA_PendingStreamTransfers(buffer.field, Duplicate(buffer.fragmentIdx)), stream, "D2H"),
            IR_Assignment(dirtyFlag, CUDA_DirtyFlagCase.CLEAR.id)))
      }
    }

    beforeHost
  }

  def syncFlagsAfterHost() = {
    var afterHost : ListBuffer[IR_Statement] = ListBuffer()

    // update flags for written fields/buffers (check for valid fragment already implicitly done in surrounding loop)
    for (access <- fieldAccesses.toSeq.sortBy(_._1)) {
      if (syncAfterHost(access._1, fieldAccesses.keys))
        afterHost += IR_Assignment(CUDA_HostDataUpdated(access._2.field, Duplicate(access._2.slot)),
          CUDA_DirtyFlagCase.INTERMEDIATE.id)
    }
    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      if (syncAfterHost(access._1, bufferAccesses.keys))
        afterHost += IR_Assignment(CUDA_HostBufferDataUpdated(access._2.field, access._2.send, Duplicate(access._2.neighIdx)),
          CUDA_DirtyFlagCase.INTERMEDIATE.id)
    }

    afterHost
  }

  def syncFlagsAfterDevice() = {
    var afterDevice : ListBuffer[IR_Statement] = ListBuffer()

    // update flags for written fields/buffers (check for valid fragment already implicitly done in surrounding loop)
    for (access <- fieldAccesses.toSeq.sortBy(_._1)) {
      if (syncAfterDevice(access._1, fieldAccesses.keys))
        afterDevice += IR_Assignment(CUDA_DeviceDataUpdated(access._2.field, Duplicate(access._2.slot), access._2.fragmentIdx),
          CUDA_DirtyFlagCase.INTERMEDIATE.id)
    }
    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      if (syncAfterDevice(access._1, bufferAccesses.keys))
        afterDevice += IR_Assignment(CUDA_DeviceBufferDataUpdated(access._2.field, access._2.send, Duplicate(access._2.neighIdx), access._2.fragmentIdx),
          CUDA_DirtyFlagCase.INTERMEDIATE.id)
    }

    afterDevice
  }

  def syncEventsBeforeDevice(stream : CUDA_Stream) = {
    var beforeDevice : ListBuffer[IR_Statement] = ListBuffer()

    // wait for pending transfer events if dirty flag is set and then reset flag
    for (access <- fieldAccesses.toSeq.sortBy(_._1)) {
      val fieldData = access._2
      if (syncBeforeDevice(access._1, fieldAccesses.keys)) {
        val field = fieldData.field
        val fragIdx = fieldData.fragmentIdx
        val domainIdx = field.domain.index
        val dirtyFlag = CUDA_HostDataUpdated(field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx))
        val isValid = CUDA_DirtyFlagHelper.fragmentIdxIsValid(fragIdx, domainIdx)
        beforeDevice += IR_IfCondition(isValid AndAnd (dirtyFlag EqEq CUDA_DirtyFlagCase.INTERMEDIATE.id),
          ListBuffer[IR_Statement](
            CUDA_WaitEvent(CUDA_PendingStreamTransfers(field, Duplicate(fieldData.fragmentIdx)), stream, "H2D"),
            IR_Assignment(dirtyFlag, CUDA_DirtyFlagCase.CLEAR.id)))
      }
    }
    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      val buffer = access._2
      if (syncBeforeDevice(access._1, bufferAccesses.keys)) {
        val field = buffer.field
        val fragIdx = buffer.fragmentIdx
        val domainIdx = field.domain.index
        val dirtyFlag = CUDA_HostBufferDataUpdated(buffer.field, buffer.send, Duplicate(buffer.neighIdx))
        val isValid = CUDA_DirtyFlagHelper.fragmentIdxIsValid(fragIdx, domainIdx)
        beforeDevice += IR_IfCondition(isValid AndAnd (dirtyFlag EqEq CUDA_DirtyFlagCase.INTERMEDIATE.id),
          ListBuffer[IR_Statement](
            CUDA_WaitEvent(CUDA_PendingStreamTransfers(buffer.field, Duplicate(buffer.fragmentIdx)), stream, "H2D"),
            IR_Assignment(dirtyFlag, CUDA_DirtyFlagCase.CLEAR.id)))
      }
    }

    beforeDevice
  }
}
