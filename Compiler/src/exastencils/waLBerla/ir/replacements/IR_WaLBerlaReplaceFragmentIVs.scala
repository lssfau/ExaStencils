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

    // fragment positions
    case assign @ IR_Assignment(fragPos, _, "=") if inWaLBerlaBlockLoop(collector) =>
      fragPos match {
        case _ @ IR_IV_FragmentPosition(dim, _)      => assign.src = getBlockAABB.center(dim)
        case _ @ IR_IV_FragmentPositionBegin(dim, _) => assign.src = getBlockAABB.min(dim)
        case _ @ IR_IV_FragmentPositionEnd(dim, _)   => assign.src = getBlockAABB.max(dim)
        case _                                       =>
      }
      assign


    // fragment connectivity
    case assign @ IR_Assignment(fragCon, _, "=") if inWaLBerlaBlockLoop(collector) =>
      fragCon match {
        case _ @ IR_IV_NeighborIsValid(_, neighIdx, fragmentIdx)     => assign.src = IR_WaLBerlaNeighborIsValid(neighIdx, fragmentIdx)
        case _ @ IR_IV_NeighborIsRemote(_, neighIdx, fragmentIdx)    => assign.src = IR_WaLBerlaNeighborIsRemote(neighIdx, fragmentIdx)
        case _ @ IR_IV_NeighborFragmentIdx(_, neighIdx, fragmentIdx) => assign.src = IR_WaLBerlaNeighborFragmentIdx(neighIdx, fragmentIdx)
        case _ @ IR_IV_NeighborRemoteRank(_, neighIdx, fragmentIdx)  => assign.src = IR_WaLBerlaNeighborRemoteRank(neighIdx, fragmentIdx)
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
