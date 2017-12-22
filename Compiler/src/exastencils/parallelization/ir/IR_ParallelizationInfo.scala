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

/**
  * @param potentiallyParallel spcifies if the loop attached can be executed in parallel
  * @param isInnermost         specifies if the loop attached is the innermost in a loop nest AND it should be optimized as such
  * @param isVectorizable      specifies if the loop attached can safely vectorized, even if it is not parallel
  * @param collapseDepth       specifies how many nested loops can be parallelized
  * @param privateVars         variables for which each thread should have a private storage
  * @param reduction           identifies a reduction, if some
  */
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
