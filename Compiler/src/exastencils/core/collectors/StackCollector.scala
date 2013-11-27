package exastencils.core.collectors

import scala.collection.mutable.Stack
import exastencils.datastructures._

class StackCollector extends Collector {
  private val stack = new Stack[Node]

  def enter(node : Node) : Unit = {
    stack.push(node)
  }
  def leave(node : Node) : Unit = {
    if (head != node) sys.exit(-1) // fatal error is fatal // FIXME replace this with some nicer error
    stack.pop()
  }
  def reset() : Unit = { stack.clear }

  def isEmpty : Boolean = { return stack.isEmpty }
  def head : Node = { return stack.head }
}
