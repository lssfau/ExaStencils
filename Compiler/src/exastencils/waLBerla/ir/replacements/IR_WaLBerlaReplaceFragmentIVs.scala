package exastencils.waLBerla.ir.replacements

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.datastructures.Transformation
import exastencils.domain.ir._
import exastencils.waLBerla.ir.blockforest._
import exastencils.waLBerla.ir.grid.IR_WaLBerlaBlockAABB

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
        case _ @ IR_IV_NeighborIsValid(_, neighIdx, fragmentIdx)     => assign.dest = IR_WaLBerlaNeighborIsValid(neighIdx, fragmentIdx)
        case _ @ IR_IV_NeighborIsRemote(_, neighIdx, fragmentIdx)    => assign.dest = IR_WaLBerlaNeighborIsRemote(neighIdx, fragmentIdx)
        case _ @ IR_IV_NeighborFragmentIdx(_, neighIdx, fragmentIdx) => assign.dest = IR_WaLBerlaNeighborFragmentIdx(neighIdx, fragmentIdx)
        case _ @ IR_IV_NeighborRemoteRank(_, neighIdx, fragmentIdx)  => assign.dest = IR_WaLBerlaNeighborRemoteRank(neighIdx, fragmentIdx)
        case _                                                       =>
      }
      assign

    /* accesses */

    // fragment positions
    case _ @ IR_IV_FragmentPosition(dim, _)      if inWaLBerlaBlockLoop(collector) => getBlockAABB.center(dim)
    case _ @ IR_IV_FragmentPositionBegin(dim, _) if inWaLBerlaBlockLoop(collector) => getBlockAABB.min(dim)
    case _ @ IR_IV_FragmentPositionEnd(dim, _)   if inWaLBerlaBlockLoop(collector) => getBlockAABB.max(dim)

    // fragment connectivity
    case _ @ IR_IV_NeighborIsValid(_, neighIdx, fragmentIdx)     if inWaLBerlaBlockLoop(collector) => IR_WaLBerlaNeighborIsValid(neighIdx, fragmentIdx)
    case _ @ IR_IV_NeighborIsRemote(_, neighIdx, fragmentIdx)    if inWaLBerlaBlockLoop(collector) => IR_WaLBerlaNeighborIsRemote(neighIdx, fragmentIdx)
    case _ @ IR_IV_NeighborFragmentIdx(_, neighIdx, fragmentIdx) if inWaLBerlaBlockLoop(collector) => IR_WaLBerlaNeighborFragmentIdx(neighIdx, fragmentIdx)
    case _ @ IR_IV_NeighborRemoteRank(_, neighIdx, fragmentIdx)  if inWaLBerlaBlockLoop(collector) => IR_WaLBerlaNeighborRemoteRank(neighIdx, fragmentIdx)

    /* conditions */

  }, recursive = false)
}
