package exastencils.util.l3

import exastencils.base.l3.L3_Node
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger._

class L3_StackCollector extends Collector {
  var stack : List[L3_Node] = Nil

  def isEmpty : Boolean = { stack.isEmpty }
  def head : L3_Node = { stack.head }

  override def enter(node : Node) : Unit = {
    node match {
      case n : L3_Node => stack ::= n
      case _           =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case n : L3_Node =>
        if (head ne n) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $n") // fatal error is fatal
        stack = stack.tail

      case _ =>
    }
  }

  override def reset() : Unit = {
    stack = Nil
  }
}
