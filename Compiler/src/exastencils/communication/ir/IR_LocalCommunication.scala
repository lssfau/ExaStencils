package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._

/// IR_LocalCommunication

abstract class IR_LocalCommunication extends IR_Statement with IR_Expandable {
  def insideFragLoop : Boolean

  def wrapFragLoop(toWrap : IR_Statement, parallel : Boolean) : IR_Statement = {
    if (insideFragLoop) {
      toWrap
    } else {
      val loop = IR_LoopOverFragments(toWrap)
      loop.parallelization.potentiallyParallel = parallel
      loop
    }
  }

  def wrapFragLoop(toWrap : ListBuffer[IR_Statement], parallel : Boolean) : ListBuffer[IR_Statement] = {
    if (insideFragLoop) {
      toWrap
    } else {
      val loop = new IR_LoopOverFragments(toWrap)
      loop.parallelization.potentiallyParallel = parallel
      ListBuffer[IR_Statement](loop)
    }
  }
}
