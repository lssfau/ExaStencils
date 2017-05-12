package exastencils.parallelization.api.cuda

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.communication.ir._
import exastencils.datastructures._

/// CUDA_GatherBufferAccess

object CUDA_GatherBufferAccess extends QuietDefaultStrategy("Gather local buffer access nodes") {
  var bufferAccesses = HashMap[String, IR_IV_CommBuffer]()
  var inWriteOp = false

  def clear() = {
    bufferAccesses.clear()
    inWriteOp = false
  }

  def mapBuffer(buffer : IR_IV_CommBuffer) = {
    var identifier = buffer.resolveName()
    identifier = (if (inWriteOp) "write_" else "read_") + identifier

    bufferAccesses.put(identifier, buffer)
  }

  this += new Transformation("Searching", {
    case assign : IR_Assignment =>
      inWriteOp = true
      CUDA_GatherBufferAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
      inWriteOp = false
      if ("=" != assign.op) // add compound assignments as read and write accesses
        CUDA_GatherBufferAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
      CUDA_GatherBufferAccess.applyStandalone(IR_ExpressionStatement(assign.src))
      assign

    case buffer : IR_IV_CommBuffer =>
      mapBuffer(buffer)
      buffer
  }, false)
}
