package exastencils.core.collectors

import scala.collection.mutable.Stack
import exastencils.core._
import exastencils.datastructures._

class StackCollector extends Collector {
  private val stack_ = new Stack[Node]
  
  def enter(node : Node) : Unit = {
    stack_.push(node)
  }
  def leave(node : Node) : Unit = {
    if (head != node) ERROR(s"StackCollector mismatch: Cannot leave(): head != $node") // fatal error is fatal
    stack_.pop()
  }
  def reset() : Unit = { stack_.clear }

  def isEmpty : Boolean = { return stack_.isEmpty }
  def head : Node = { return stack_.head }
  
  def stack = stack_
  def list = stack_.toList
}
