package exastencils.parallelization.ir

import exastencils.base.ir._

/// IR_ParallelizationInfo

object IR_ParallelizationInfo {
  def apply() = new IR_ParallelizationInfo(false, 1, None)

  def PotentiallyParallel() = {
    val info = this ()
    info.potentiallyParallel = true
    info
  }
}

case class IR_ParallelizationInfo(
    var potentiallyParallel : Boolean,
    var collapseDepth : Int,
    var reduction : Option[IR_Reduction]
) extends IR_Node {}

/// IR_HasParallelizationInfo

trait IR_HasParallelizationInfo {
  var parallelization : IR_ParallelizationInfo
}