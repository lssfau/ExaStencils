package exastencils.core.collectors

import scala.collection.mutable.Stack

import exastencils.datastructures.Node
import exastencils.logger._

class StackCollector extends Collector {
  var stack : List[Node] = Nil

  def isEmpty : Boolean = { stack.isEmpty }
  def head : Node = { stack.head }

  override def enter(node : Node) : Unit = {
    stack ::= node
  }

  override def leave(node : Node) : Unit = {
    if (head ne node) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $node") // fatal error is fatal
    stack = stack.tail
  }

  override def reset() : Unit = {
    stack = Nil
  }
}
