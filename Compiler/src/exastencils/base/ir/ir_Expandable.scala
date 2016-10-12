package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._

/// IR_Expandable

trait IR_Expandable {
  def expand() : Transformation.OutputType
}

/// IR_Expand

object IR_Expand extends DefaultStrategy("Expand all applicable nodes") {
  def doUntilDone(node : Option[Node] = None) = {
    do { apply(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node) = {
    do { applyStandalone(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
  }

  this += new Transformation("Expand", {
    case expandable : IR_Expandable => expandable.expand()
  })
}

/// IR_ExpandInOnePass

// TODO: this strategy becomes somewhat obsolete as soon as trafos implement the required behavior directly
object IR_ExpandInOnePass extends DefaultStrategy("Expand all applicable nodes") {
  this += new Transformation("Expand", {
    case expandable : IR_Expandable =>
      var nodes : ListBuffer[Node] = ListBuffer()
      nodes += expandable
      var expandedSth = false
      do {
        expandedSth = false
        for (n <- nodes.indices) {
          if (!expandedSth) {
            nodes(n) match {
              case expandable : IR_Expandable =>
                val output = expandable.expand()
                output.inner match {
                  case single : Node   => nodes.update(n, single)
                  case list : NodeList =>
                    val split = nodes.splitAt(n)
                    split._2.remove(0)
                    nodes = split._1 ++ list.nodes ++ split._2
                }
                expandedSth = true
              case _                          =>
            }
          }
        }
      } while (expandedSth)

      if (nodes.length == 1)
        nodes(0)
      else
        nodes
  })
}
