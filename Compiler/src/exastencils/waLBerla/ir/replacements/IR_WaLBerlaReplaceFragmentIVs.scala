package exastencils.waLBerla.ir.replacements

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.communication.ir.IR_IV_CommunicationId
import exastencils.datastructures.Transformation
import exastencils.domain.ir._
import exastencils.waLBerla.ir.blockforest._
import exastencils.waLBerla.ir.communication.IR_WaLBerlaCommunicationId
import exastencils.waLBerla.ir.grid._
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaRefinementCase
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaRefinementIndexForCoarseNeighbor

object IR_WaLBerlaReplaceFragmentIVs extends IR_WaLBerlaReplacementStrategy("Replace frag info accesses with accesses to waLBerla block info") {
  def block = IR_WaLBerlaLoopOverLocalBlocks.block

  def getBlockAABB = IR_WaLBerlaBlockAABB(block)

  this += Transformation("Replace non-recursive", {

    /* assignments */

    case assign @ IR_Assignment(fragIV, _, "=") if inWaLBerlaScope(collector) =>
      fragIV match {
        // fragment positions
        case _ @ IR_IV_FragmentPosition(dim, _)      => assign.src = IR_WaLBerlaAABBCenter(getBlockAABB, dim)
        case _ @ IR_IV_FragmentPositionBegin(dim, _) => assign.src = IR_WaLBerlaAABBMin(getBlockAABB, dim)
        case _ @ IR_IV_FragmentPositionEnd(dim, _)   => assign.src = IR_WaLBerlaAABBMax(getBlockAABB, dim)
        // fragment connectivity
        case _ @ IR_IV_NeighborIsValid(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx)     =>
          assign.dest = IR_WaLBerlaNeighborIsValid(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
        case _ @ IR_IV_NeighborIsRemote(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx)    =>
          assign.dest = IR_WaLBerlaNeighborIsRemote(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
        case _ @ IR_IV_NeighborFragmentIdx(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx) =>
          assign.dest = IR_WaLBerlaNeighborFragmentIdx(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
        case _ @ IR_IV_NeighborRemoteRank(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx)  =>
          assign.dest = IR_WaLBerlaNeighborRemoteRank(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
        case _ @ IR_IV_CommunicationId(fragmentIdx)                                          =>
          assign.dest = IR_WaLBerlaCommunicationId(fragmentIdx)
        case _                                                                               =>
      }
      assign

    // refinement info
    case assign @ IR_Assignment(_ @ IR_IV_NeighborRefinementCase(fragIdx, _, neighIdx), _, "=") if inWaLBerlaScope(collector) =>
      assign.dest = IR_WaLBerlaRefinementCase(fragIdx, neighIdx)
      assign

    case assign @ IR_Assignment(_ @ IR_RefinementIndexForCoarseNeighbor(neighIdx, _, fragIdx), _, "=") if inWaLBerlaScope(collector) =>
      assign.dest = IR_WaLBerlaRefinementIndexForCoarseNeighbor(neighIdx, fragIdx)
      assign

    /* accesses */

    // fragment positions
    case _ @ IR_IV_FragmentPosition(dim, _) if inWaLBerlaScope(collector)      => IR_WaLBerlaAABBCenter(getBlockAABB, dim)
    case _ @ IR_IV_FragmentPositionBegin(dim, _) if inWaLBerlaScope(collector) => IR_WaLBerlaAABBMin(getBlockAABB, dim)
    case _ @ IR_IV_FragmentPositionEnd(dim, _) if inWaLBerlaScope(collector)   => IR_WaLBerlaAABBMax(getBlockAABB, dim)

  }, recursive = false)

  this += Transformation("Replace recursive", {

    /* accesses */

    // fragment connectivity
    case _ @ IR_IV_NeighborIsValid(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx) if inWaLBerlaScope(collector)     =>
      IR_WaLBerlaNeighborIsValid(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ IR_IV_NeighborIsRemote(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx) if inWaLBerlaScope(collector)    =>
      IR_WaLBerlaNeighborIsRemote(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ IR_IV_NeighborFragmentIdx(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx) if inWaLBerlaScope(collector) =>
      IR_WaLBerlaNeighborFragmentIdx(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ IR_IV_NeighborRemoteRank(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx) if inWaLBerlaScope(collector)  =>
      IR_WaLBerlaNeighborRemoteRank(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ IR_IV_CommunicationId(fragmentIdx) if inWaLBerlaScope(collector)                                          =>
      IR_WaLBerlaCommunicationId(fragmentIdx)

    // refinement info
    case _ @ IR_IV_NeighborRefinementCase(fragIdx, _, neighIdx) if inWaLBerlaScope(collector) =>
      IR_WaLBerlaRefinementCase(fragIdx, neighIdx)
    case _ @ IR_RefinementIndexForCoarseNeighbor(neighIdx, _, fragIdx) if inWaLBerlaScope(collector) =>
      IR_WaLBerlaRefinementIndexForCoarseNeighbor(neighIdx, fragIdx)

    /* conditions */
  })
}
