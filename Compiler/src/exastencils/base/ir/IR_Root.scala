package exastencils.base.ir

import scala.collection.mutable.ListBuffer

/// IR_Root

object IR_Root {
  def apply(nodes : IR_Node*) = new IR_Root(nodes.to[ListBuffer])
}

case class IR_Root(var nodes : ListBuffer[IR_Node]) extends IR_Node {

  def +=(n : IR_Node*) : IR_Root = {
    nodes ++= n
    this
  }

  // resolve nested root nodes
  def flatten() : Unit = {
    while (nodes.exists(_.isInstanceOf[IR_Root]))
      nodes = nodes.flatMap {
        case root : IR_Root => root.nodes
        case other          => ListBuffer(other)
      }
  }
}
