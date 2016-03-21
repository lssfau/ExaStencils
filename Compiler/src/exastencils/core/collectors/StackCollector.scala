package exastencils.core.collectors

import scala.collection.mutable.Stack

import exastencils.datastructures.Node
import exastencils.logger._

class StackCollector extends Collector {
  final val stack = new Stack[Node]()

  def isEmpty : Boolean = { return stack.isEmpty }
  def head : Node = { return stack.head }

  override def enter(node : Node) : Unit = {
    stack.push(node)
  }

  override def leave(node : Node) : Unit = {
    if (head ne node) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $node") // fatal error is fatal
    stack.pop()
  }

  override def reset() : Unit = {
    stack.clear
  }
}
