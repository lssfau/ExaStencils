package exastencils.waLBerla.ir.replacements

import exastencils.communication.ir._
import exastencils.datastructures.Transformation
import exastencils.parallelization.api.mpi.MPI_Request
import exastencils.parallelization.api.mpi.MPI_RequestNoField
import exastencils.waLBerla.ir.communication._

object IR_WaLBerlaReplaceCommIVs extends IR_WaLBerlaReplacementStrategy("Replace comm IV accesses to wb counterparts") {

  this += Transformation("Replace", {
    case _ @ IR_IV_CommBuffer(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)        =>
      IR_WaLBerlaCommBuffer(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ IR_IV_CommBufferBasePtr(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx) =>
      IR_WaLBerlaCommBufferBasePtr(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ IR_IV_CommBufferIterator(field, send, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)      =>
      IR_WaLBerlaCommBufferIterator(field, send, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)

    case _ @ MPI_Request(field, send, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx) =>
      IR_WaLBerlaMPIRequest(field, send, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ MPI_RequestNoField(send, neighIdx, fragmentIdx)                                        =>
      IR_WaLBerlaMPIRequestNoField(send, neighIdx, fragmentIdx)

    case _ @ IR_IV_RemoteReqOutstanding(field, send, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx) =>
      IR_WaLBerlaRemoteReqOutstanding(field, send, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ IR_IV_RemoteReqOutstandingNoField(send, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx) =>
      IR_WaLBerlaRemoteReqOutstandingNoField(send, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)

  }, recursive = false)
}