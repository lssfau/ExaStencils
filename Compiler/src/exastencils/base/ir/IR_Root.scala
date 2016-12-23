package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Node

/// IR_Root

object IR_Root {
  def apply(nodes : Node*) = new IR_Root(nodes.to[ListBuffer])
}

case class IR_Root(var nodes : ListBuffer[Node]) extends IR_Node {
  def +=(n : Node*) = nodes ++= n
}
