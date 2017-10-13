package exastencils.util.l4

import exastencils.base.l4.L4_Node
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger._

class L4_StackCollector extends Collector {
  var stack : List[L4_Node] = Nil

  def isEmpty : Boolean = { stack.isEmpty }
  def head : L4_Node = { stack.head }

  override def enter(node : Node) : Unit = {
    node match {
      case n : L4_Node => stack ::= n
      case _           =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case n : L4_Node =>
        if (head ne n) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $n") // fatal error is fatal
        stack = stack.tail

      case _ =>
    }
  }

  override def reset() : Unit = {
    stack = Nil
  }
}
