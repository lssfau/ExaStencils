package exastencils.parallelization.api.cuda

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ScopedStatement
import exastencils.base.ir.IR_Statement
import exastencils.communication.ir.IR_IV_CommBuffer
import exastencils.core.Duplicate
import exastencils.field.ir.IR_IV_FieldData
import exastencils.logger.Logger
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.util.ir.IR_CommunicationKernelCollector
import exastencils.util.ir.IR_FragmentLoopCollector

trait CUDA_PrepareFragmentLoops extends CUDA_PrepareBufferSync {

  def fieldAccesses : mutable.Map[String, IR_IV_FieldData]
  def bufferAccesses : mutable.Map[String, IR_IV_CommBuffer]

  def accessedElementsFragLoop : mutable.HashMap[IR_ScopedStatement with IR_HasParallelizationInfo, CUDA_AccessedElementsInFragmentLoop]

  def createFragLoopHandler(loop : IR_ScopedStatement with IR_HasParallelizationInfo) = {
    if (accessedElementsFragLoop.contains(loop)) {
      Logger.warn("Found fragment loop to adapt")
      val accessedElements = accessedElementsFragLoop(loop)
      CUDA_HandleFragmentLoops(loop, Duplicate(accessedElements))
    } else {
      loop
    }
  }

  def collectAccessedBuffers(stmts : IR_Statement*) : Unit = {}

  def collectAccessedElementsFragmentLoop(
      body : ListBuffer[IR_Statement],
      fragLoopCollector: IR_FragmentLoopCollector,
      commKernelCollector : IR_CommunicationKernelCollector) = {

    // collect elements accessed in enclosing fragment loop and create handler
    val enclosingFragLoop = fragLoopCollector.getEnclosingFragmentLoop()
    if (enclosingFragLoop.isDefined) {
      val emptyElements = CUDA_AccessedElementsInFragmentLoop(ListBuffer(), mutable.HashMap(), mutable.HashMap())
      val elements = accessedElementsFragLoop.getOrElse(enclosingFragLoop.get, emptyElements)

      // get accessed buffers
      collectAccessedBuffers(body : _*)

      // determine execution ( = comm/comp ) stream
      val executionStream = CUDA_Stream.getStream(fragLoopCollector, commKernelCollector)

      // add accessed streams
      if (!elements.streams.contains(executionStream))
        elements.streams += executionStream

      // add accessed buffers/fields
      elements.fieldAccesses ++= Duplicate(fieldAccesses.map { case (str, fAcc) => str -> IR_IV_FieldData(fAcc.field, fAcc.slot, fAcc.fragmentIdx) })
      elements.bufferAccesses ++= Duplicate(bufferAccesses)

      accessedElementsFragLoop.update(enclosingFragLoop.get, elements)
    }
  }

  def syncEventsBeforeHost(stream : CUDA_Stream) = {
    var beforeHost : ListBuffer[IR_Statement] = ListBuffer()

    // wait for pending transfer events
    for (access <- fieldAccesses.toSeq.sortBy(_._1)) {
      val fieldData = access._2
      if (syncBeforeHost(access._1, fieldAccesses.keys))
        beforeHost += CUDA_WaitEvent(CUDA_PendingStreamTransfers(fieldData.field, fieldData.fragmentIdx), stream, "D2H")
    }
    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      val buffer = access._2
      if (syncBeforeHost(access._1, bufferAccesses.keys))
        beforeHost += CUDA_WaitEvent(CUDA_PendingStreamTransfers(buffer.field, buffer.fragmentIdx), stream, "D2H")
    }

    beforeHost
  }

  def syncEventsBeforeDevice(stream : CUDA_Stream) = {
    var beforeDevice : ListBuffer[IR_Statement] = ListBuffer()

    // wait for pending transfer events
    for (access <- fieldAccesses.toSeq.sortBy(_._1)) {
      val fieldData = access._2
      if (syncBeforeDevice(access._1, fieldAccesses.keys))
        beforeDevice += CUDA_WaitEvent(CUDA_PendingStreamTransfers(fieldData.field, fieldData.fragmentIdx), stream, "H2D")
    }
    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      val buffer = access._2
      if (syncBeforeDevice(access._1, bufferAccesses.keys))
        beforeDevice += CUDA_WaitEvent(CUDA_PendingStreamTransfers(buffer.field, buffer.fragmentIdx), stream, "H2D")
    }

    beforeDevice
  }
}
