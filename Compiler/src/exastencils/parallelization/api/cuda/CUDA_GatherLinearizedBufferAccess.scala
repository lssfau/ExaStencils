package exastencils.parallelization.api.cuda

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.communication.ir._
import exastencils.datastructures._

/// CUDA_GatherLinearizedBufferAccess

object CUDA_GatherLinearizedBufferAccess extends QuietDefaultStrategy("Gather local buffer access nodes") {
  var bufferAccesses = HashMap[String, IR_IV_CommBuffer]()

  def clear() = {
    bufferAccesses.clear()
  }

  this += new Transformation("Searching", {
    case access @ IR_ArrayAccess(buffer : IR_IV_CommBuffer, _, _) =>
      bufferAccesses.put(buffer.resolveName(), buffer)

      access
  }, false)
}

/// CUDA_ReplaceLinearizedBufferAccess

object CUDA_ReplaceLinearizedBufferAccess extends QuietDefaultStrategy("Replace local LinearizedBufferAccess nodes") {
  var bufferAccesses = HashMap[String, IR_IV_CommBuffer]()

  this += new Transformation("Searching", {
    case access @ IR_ArrayAccess(buffer : IR_IV_CommBuffer, _, _) =>
      IR_ArrayAccess(IR_VariableAccess(buffer.resolveName(), buffer.field.resolveDeclType), access.index)
  })
}
