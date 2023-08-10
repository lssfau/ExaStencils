package exastencils.util.ir

import scala.collection.mutable

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.core.StateManager
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.domain.ir.IR_IV_NeighborIsValid
import exastencils.logger.Logger
import exastencils.parallelization.ir.IR_HasParallelizationInfo

class IR_CommunicationKernelCollector extends Collector {

  private var fragmentLoopStack : List[IR_ScopedStatement with IR_HasParallelizationInfo] = Nil
  private var communicationInFragmentLoop : mutable.HashMap[IR_ScopedStatement with IR_HasParallelizationInfo, IR_Expression] = mutable.HashMap()

  def contains(fragLoop : IR_ScopedStatement with IR_HasParallelizationInfo) = communicationInFragmentLoop.contains(fragLoop)
  def getNeighbor(fragLoop : IR_ScopedStatement with IR_HasParallelizationInfo) = {
    if (contains(fragLoop))
      Some(communicationInFragmentLoop(fragLoop))
    else
      None
  }

  def isNeighborIdx(expr : IR_Expression) = communicationInFragmentLoop.values.toSeq.contains(expr)
  def isEmpty : Boolean = { communicationInFragmentLoop.isEmpty }
  private def head : IR_ScopedStatement with IR_HasParallelizationInfo = { fragmentLoopStack.head }

  override def enter(node : Node) : Unit = {
    node match {
      case loop : IR_LoopOverFragments =>
        fragmentLoopStack ::= loop

      case loop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == IR_LoopOverFragments.defIt.name =>
        fragmentLoopStack ::= loop

      // TODO: extend this list in case that other conditions are used in communication/boundary handling

      // communication/boundary handling
      case cond : IR_IfCondition if fragmentLoopStack != Nil =>
        val neigh = StateManager.findFirst[IR_IV_NeighborIsValid](cond)
        if (neigh.isDefined)
          communicationInFragmentLoop += (head -> neigh.get.neighIdx)

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case loop : IR_LoopOverFragments =>
        if (head ne loop) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $loop") // fatal error
        fragmentLoopStack = fragmentLoopStack.tail

      case loop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == IR_LoopOverFragments.defIt.name =>
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
