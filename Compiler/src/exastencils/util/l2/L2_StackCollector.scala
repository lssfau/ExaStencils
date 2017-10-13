package exastencils.util.l2

import exastencils.base.l2.L2_Node
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger._

class L2_StackCollector extends Collector {
  var stack : List[L2_Node] = Nil

  def isEmpty : Boolean = { stack.isEmpty }
  def head : L2_Node = { stack.head }

  override def enter(node : Node) : Unit = {
    node match {
      case n : L2_Node => stack ::= n
      case _           =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case n : L2_Node =>
        if (head ne n) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $n") // fatal error is fatal
        stack = stack.tail

      case _ =>
    }
  }

  override def reset() : Unit = {
    stack = Nil
  }
}
