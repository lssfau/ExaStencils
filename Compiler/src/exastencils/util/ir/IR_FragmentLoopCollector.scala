package exastencils.util.ir

import scala.collection.mutable

import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_ScopedStatement
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_LoopOverProcessLocalBlocks
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.parallelization.ir.IR_HasParallelizationInfo

class IR_FragmentLoopCollector extends Collector {
  val fragmentLoops = new mutable.Stack[IR_ScopedStatement with IR_HasParallelizationInfo]

  override def enter(node : Node) : Unit = {
    node match {
      case fragLoop : IR_LoopOverFragments                                                                                     => fragmentLoops.push(fragLoop)
      case fragLoop : IR_LoopOverProcessLocalBlocks                                                                            => fragmentLoops.push(fragLoop)
      case fragLoop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == IR_LoopOverFragments.defIt.name => fragmentLoops.push(fragLoop)
      case _                                                                                                                   =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ : IR_LoopOverFragments                                                                                     => fragmentLoops.pop()
      case _ : IR_LoopOverProcessLocalBlocks                                                                            => fragmentLoops.pop()
      case _ @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == IR_LoopOverFragments.defIt.name => fragmentLoops.pop()
      case _                                                                                                            =>
    }
  }

  override def reset() : Unit = {
    fragmentLoops.clear
  }

  def getEnclosingFragmentLoop() : Option[IR_ScopedStatement with IR_HasParallelizationInfo] = {
    fragmentLoops.headOption
  }
}
