package exastencils.datastructures.ir

import exastencils.datastructures._
import scala.collection.mutable.ListBuffer
import scala.collection.Traversable

case class Root(var nodes : ListBuffer[Node]) extends Node {
  def this() = this(ListBuffer[Node]())
  def this(t : Traversable[Node]) = { this(); nodes ++= t }

  def +=(node : Node) = { nodes += node }
  def +=(n : Traversable[Node]) = { nodes ++= n }
}
