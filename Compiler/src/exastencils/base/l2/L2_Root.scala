package exastencils.base.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_Root
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L2_Root

object L2_Root {
  def apply(node : L2_Node) = new L2_Root(ListBuffer(node))
  def apply(nodes : List[L2_Node]) = new L2_Root(nodes.to[ListBuffer])
}

case class L2_Root(var nodes : ListBuffer[L2_Node]) extends L2_Node with L2_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = {
    // TODO: print knowledge collections
    nodes.foreach {
      case p : PrettyPrintable => out << p << "\n\n"
      case _                   =>
    }
  }

  override def progress = {
    val progressableNodes = ListBuffer[L2_Progressable]()
    nodes.foreach {
      case node : L2_Progressable => progressableNodes += node
      case node                   => Logger.warn("Found un-progressable node " + node)
    }
    L3_Root(progressableNodes.map(_.progress))
  }
}
