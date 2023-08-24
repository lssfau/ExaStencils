package exastencils.waLBerla.ir.replacements

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.datastructures.Transformation
import exastencils.domain.ir._
import exastencils.waLBerla.ir.blockforest._
import exastencils.waLBerla.ir.grid.IR_WaLBerlaBlockAABB
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaRefinementCase

object IR_WaLBerlaReplaceFragmentIVs extends IR_WaLBerlaReplacementStrategy("Replace frag info accesses with accesses to waLBerla block info") {
  def block = IR_WaLBerlaLoopOverLocalBlocks.block

  def getBlockAABB = IR_WaLBerlaBlockAABB(block)

  this += Transformation("Replace", {

    /* assignments */

    case assign @ IR_Assignment(fragIV, _, "=") if inWaLBerlaBlockLoop(collector) =>
      fragIV match {
        // fragment positions
        case _ @ IR_IV_FragmentPosition(dim, _)      => assign.src = getBlockAABB.center(dim)
        case _ @ IR_IV_FragmentPositionBegin(dim, _) => assign.src = getBlockAABB.min(dim)
        case _ @ IR_IV_FragmentPositionEnd(dim, _)   => assign.src = getBlockAABB.max(dim)
        // fragment connectivity
        case _ @ IR_IV_NeighborIsValid(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx)     =>
          assign.dest = IR_WaLBerlaNeighborIsValid(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
        case _ @ IR_IV_NeighborIsRemote(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx)    =>
          assign.dest = IR_WaLBerlaNeighborIsRemote(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
        case _ @ IR_IV_NeighborFragmentIdx(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx) =>
          assign.dest = IR_WaLBerlaNeighborFragmentIdx(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
        case _ @ IR_IV_NeighborRemoteRank(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx)  =>
          assign.dest = IR_WaLBerlaNeighborRemoteRank(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
        case _                                                          =>
      }
      assign

    // refinement case
    case assign @ IR_Assignment(_ @ IR_IV_NeighborRefinementCase(fragIdx, _, neighIdx), _, "=") if inWaLBerlaBlockLoop(collector) =>
      assign.dest = IR_WaLBerlaRefinementCase(fragIdx, neighIdx)
      assign

    /* accesses */

    // fragment positions
    case _ @ IR_IV_FragmentPosition(dim, _) if inWaLBerlaBlockLoop(collector)      => getBlockAABB.center(dim)
    case _ @ IR_IV_FragmentPositionBegin(dim, _) if inWaLBerlaBlockLoop(collector) => getBlockAABB.min(dim)
    case _ @ IR_IV_FragmentPositionEnd(dim, _) if inWaLBerlaBlockLoop(collector)   => getBlockAABB.max(dim)

    // fragment connectivity
    case _ @ IR_IV_NeighborIsValid(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx) if inWaLBerlaBlockLoop(collector)     =>
      IR_WaLBerlaNeighborIsValid(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ IR_IV_NeighborIsRemote(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx) if inWaLBerlaBlockLoop(collector)    =>
      IR_WaLBerlaNeighborIsRemote(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ IR_IV_NeighborFragmentIdx(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_WaLBerlaNeighborFragmentIdx(neighIdx, indexOfRefinedNeighbor, fragmentIdx)
    case _ @ IR_IV_NeighborRemoteRank(_, neighIdx, indexOfRefinedNeighbor, fragmentIdx) if inWaLBerlaBlockLoop(collector)  =>
      IR_WaLBerlaNeighborRemoteRank(neighIdx, indexOfRefinedNeighbor, fragmentIdx)

    // refinement case
    case _ @ IR_IV_NeighborRefinementCase(fragIdx, _, neighIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_WaLBerlaRefinementCase(fragIdx, neighIdx)

    /* conditions */

  }, recursive = false)
}
