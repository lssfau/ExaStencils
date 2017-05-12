package exastencils.base.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_Root
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L3_Root

object L3_Root {
  def apply(node : L3_Node) = new L3_Root(ListBuffer(node))
  def apply(nodes : List[L3_Node]) = new L3_Root(nodes.to[ListBuffer])
}

case class L3_Root(var nodes : ListBuffer[L3_Node]) extends L3_Node with L3_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = {
    // TODO: print knowledge collections
    nodes.foreach {
      case p : PrettyPrintable => out << p << "\n\n"
      case _                   =>
    }
  }

  override def progress = {
    val progressableNodes = ListBuffer[L3_Progressable]()
    nodes.foreach {
      case node : L3_Progressable => progressableNodes += node
      case node                   => Logger.warn("Found un-progressable node " + node)
    }
    L4_Root(progressableNodes.map(_.progress))
  }
}
