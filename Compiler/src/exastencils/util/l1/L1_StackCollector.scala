package exastencils.util.l1

import exastencils.base.l1.L1_Node
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger._

class L1_StackCollector extends Collector {
  var stack : List[L1_Node] = Nil

  def isEmpty : Boolean = { stack.isEmpty }
  def head : L1_Node = { stack.head }

  override def enter(node : Node) : Unit = {
    node match {
      case n : L1_Node => stack ::= n
      case _           =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case n : L1_Node =>
        if (head ne n) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $n") // fatal error is fatal
        stack = stack.tail

      case _ =>
    }
  }

  override def reset() : Unit = {
    stack = Nil
  }
}
