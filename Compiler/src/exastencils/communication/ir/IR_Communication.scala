package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.domain.ir.RefinementCase
import exastencils.fieldlike.ir.IR_FieldLike

trait IR_Communication extends IR_Statement with IR_Expandable with IR_HasRefinedCommunication {

  def field : IR_FieldLike

  def insideFragLoop : Boolean

  def wrapCond(refinementCase : RefinementCase.Access, neighbor : NeighborInfo, indexOfRefinedNeighbor : Option[IR_Expression], stmt : IR_Statement) : IR_Statement

  def wrapCond(refinementCase : RefinementCase.Access, neighbor : NeighborInfo, indexOfRefinedNeighbor : Option[IR_Expression], body : ListBuffer[IR_Statement]) : IR_Statement

  def wrapFragLoop(toWrap : IR_Statement) : IR_Statement = {
    if (insideFragLoop) {
      toWrap
    } else {
      val loop = IR_LoopOverFragments(toWrap)
      loop.parallelization.potentiallyParallel = Knowledge.comm_parallelizeFragmentLoops
      loop
    }
  }

  def wrapFragLoop(toWrap : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
    if (insideFragLoop) {
      toWrap
    } else {
      val loop = new IR_LoopOverFragments(toWrap)
      loop.parallelization.potentiallyParallel = Knowledge.comm_parallelizeFragmentLoops
      ListBuffer[IR_Statement](loop)
    }
  }
}
