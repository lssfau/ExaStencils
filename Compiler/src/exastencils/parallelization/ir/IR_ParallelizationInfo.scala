package exastencils.parallelization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._

/// IR_ParallelizationInfo

/**
  * @param potentiallyParallel spcifies if the loop attached can be executed in parallel
  * @param isInnermost         specifies if the loop attached is the innermost in a loop nest AND it should be optimized as such
  * @param isVectorizable      specifies if the loop attached can safely vectorized, even if it is not parallel
  * @param collapseDepth       specifies how many nested loops can be parallelized
  * @param privateVars         variables for which each thread should have a private storage
  * @param reduction           identifies a reduction, if some
  */
case class IR_ParallelizationInfo(
    var potentiallyParallel : Boolean = false,
    var isInnermost : Boolean = false,
    var isVectorizable : Boolean = false,
    var collapseDepth : Int = 1,
    var privateVars : ListBuffer[IR_VariableAccess] = ListBuffer(),
    var reduction : Option[IR_Reduction] = None
) extends IR_Node {}

/// IR_HasParallelizationInfo

trait IR_HasParallelizationInfo {
  var parallelization : IR_ParallelizationInfo
}
