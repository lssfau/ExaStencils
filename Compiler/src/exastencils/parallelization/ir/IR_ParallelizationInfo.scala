package exastencils.parallelization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._

/// IR_ParallelizationInfo

object IR_ParallelizationInfo {
  def apply() = new IR_ParallelizationInfo(false, false, false, 1, ListBuffer(), None)

  def PotentiallyParallel() = {
    val info = this ()
    info.potentiallyParallel = true
    info
  }

  def Vectorizable() = {
    val info = this ()
    info.isVectorizable = true
    info
  }
}

case class IR_ParallelizationInfo(
    var potentiallyParallel : Boolean,
    var isInnermost : Boolean,
    var isVectorizable : Boolean,
    var collapseDepth : Int,
    var privateVars : ListBuffer[IR_VariableAccess],
    var reduction : Option[IR_Reduction]
) extends IR_Node {}

/// IR_HasParallelizationInfo

trait IR_HasParallelizationInfo {
  var parallelization : IR_ParallelizationInfo
}
