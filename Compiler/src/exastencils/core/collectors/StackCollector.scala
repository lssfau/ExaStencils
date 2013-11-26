package exastencils.core.collectors

import scala.collection.mutable.Stack
import exastencils.datastructures._

class StackCollector extends Collector {
  private val stack = new Stack[Node]

  def enter(node : Node) : Unit = {
    println("Entering node " + node)
    stack.push(node)
  }
  def leave(node : Node) : Unit = {
    println("Leaving node " + node)
    if (head != node) println("omg error while popping " + node)
    stack.pop()
  }
  def reset() : Unit = { stack.clear }

  def isEmpty : Boolean = { return stack.isEmpty }
  def head : Node = { return stack.head }
}
