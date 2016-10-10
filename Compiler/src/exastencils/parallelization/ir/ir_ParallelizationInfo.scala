package exastencils.parallelization.ir

import exastencils.base.ir._

/// IR_ParallelizationInfo

object IR_ParallelizationInfo {
  def apply() = new IR_ParallelizationInfo(false, None)
}

case class IR_ParallelizationInfo(
    var potentiallyParallel : Boolean,
    var reduction : Option[IR_Reduction]
) extends IR_Node {

}
