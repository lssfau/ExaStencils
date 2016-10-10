package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.omp.OMP_PotentiallyParallel

/// IR_LocalCommunication

abstract class IR_LocalCommunication extends IR_Statement with IR_Expandable {
  def insideFragLoop : Boolean

  def wrapFragLoop(toWrap : IR_Statement, parallel : Boolean) : IR_Statement = {
    if (insideFragLoop)
      toWrap
    else if (parallel) {
      val loop = new IR_LoopOverFragments(ListBuffer[IR_Statement](toWrap)) with OMP_PotentiallyParallel
      loop.parallelization.potentiallyParallel = true
      loop
    } else
      IR_LoopOverFragments(toWrap)
  }

  def wrapFragLoop(toWrap : ListBuffer[IR_Statement], parallel : Boolean) : ListBuffer[IR_Statement] = {
    if (insideFragLoop)
      toWrap
    else if (parallel) {
      val loop = new IR_LoopOverFragments(toWrap) with OMP_PotentiallyParallel
      loop.parallelization.potentiallyParallel = true
      ListBuffer[IR_Statement](loop)
    } else
      ListBuffer[IR_Statement](IR_LoopOverFragments(toWrap))
  }
}
