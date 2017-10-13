package exastencils.util.ir

import exastencils.base.ir.IR_Node
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger._

class IR_StackCollector extends Collector {
  var stack : List[IR_Node] = Nil

  def isEmpty : Boolean = { stack.isEmpty }
  def head : IR_Node = { stack.head }

  override def enter(node : Node) : Unit = {
    node match {
      case n : IR_Node => stack ::= n
      case _           =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case n : IR_Node =>
        if (head ne n) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $n") // fatal error is fatal
        stack = stack.tail

      case _ =>
    }
  }

  override def reset() : Unit = {
    stack = Nil
  }
}
