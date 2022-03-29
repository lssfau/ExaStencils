package exastencils.util.ir

import scala.collection.mutable

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.domain.ir.IR_IV_NeighborIsValid
import exastencils.logger.Logger

class IR_CommunicationKernelCollector extends Collector {

  private var fragmentLoopStack : List[IR_LoopOverFragments] = Nil
  var communicationInFragmentLoop : mutable.HashMap[IR_LoopOverFragments, IR_IV_NeighborIsValid] = mutable.HashMap()

  def isEmpty : Boolean = { communicationInFragmentLoop.isEmpty }
  private def head : IR_LoopOverFragments = { fragmentLoopStack.head }

  override def enter(node : Node) : Unit = {
    node match {
      case loop : IR_LoopOverFragments =>
        fragmentLoopStack ::= loop

      // TODO: extend this list in case that other conditions are used in communication/boundary handling

      // communication
      case _ @ IR_IfCondition(neigh : IR_IV_NeighborIsValid, _, _) =>
        communicationInFragmentLoop += (head -> neigh)

      // boundary handling
      case _ @ IR_IfCondition(IR_Negation(neigh : IR_IV_NeighborIsValid), _, _) =>
        communicationInFragmentLoop += (head -> neigh)

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case loop : IR_LoopOverFragments =>
        if (head ne loop) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $loop") // fatal error
        fragmentLoopStack = fragmentLoopStack.tail

      case _ =>
    }
  }

  override def reset() : Unit = {
    fragmentLoopStack = Nil
    communicationInFragmentLoop.clear()
  }
}
