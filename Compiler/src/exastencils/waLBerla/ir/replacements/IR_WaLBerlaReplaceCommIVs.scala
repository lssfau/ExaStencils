package exastencils.waLBerla.ir.replacements

import exastencils.communication.ir.IR_IV_CommBuffer
import exastencils.communication.ir.IR_IV_CommBufferBasePtr
import exastencils.communication.ir.IR_IV_CommBufferIterator
import exastencils.datastructures.Transformation
import exastencils.waLBerla.ir.communication.IR_WaLBerlaCommBuffer
import exastencils.waLBerla.ir.communication.IR_WaLBerlaCommBufferBasePtr
import exastencils.waLBerla.ir.communication.IR_WaLBerlaCommBufferIterator

object IR_WaLBerlaReplaceCommIVs extends IR_WaLBerlaReplacementStrategy("Replace comm IV accesses to wb counterparts") {

  this += Transformation("Replace", {
    case _ @ IR_IV_CommBuffer(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx) =>
      IR_WaLBerlaCommBuffer(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)

    case _ @ IR_IV_CommBufferBasePtr(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx) =>
      IR_WaLBerlaCommBufferBasePtr(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)

    case _ @ IR_IV_CommBufferIterator(field, send, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx) =>
      IR_WaLBerlaCommBufferIterator(field, send, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)
  }, recursive = false)
}