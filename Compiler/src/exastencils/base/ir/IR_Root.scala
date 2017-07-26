package exastencils.base.ir

import scala.collection.mutable.ListBuffer

/// IR_Root

object IR_Root {
  def apply(nodes : IR_Node*) = new IR_Root(nodes.to[ListBuffer])
}

case class IR_Root(var nodes : ListBuffer[IR_Node]) extends IR_Node {
  def +=(n : IR_Node*) = nodes ++= n
}
