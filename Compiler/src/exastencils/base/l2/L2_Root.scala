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
      case other               => Logger.warn(s"Trying to print unsupported L2 node $other")
    }
  }

  override def progress = {
    val (progressable, invalid) = nodes.partition(_.isInstanceOf[L2_Progressable])
    invalid.foreach(node => Logger.warn(s"Trying to progress unsupported L2 node $node"))

    L3_Root(progressable.map(_.asInstanceOf[L2_Progressable].progress))
  }

  // resolve nested root nodes
  def flatten() = {
    while (nodes.exists(_.isInstanceOf[L2_Root]))
      nodes = nodes.flatMap {
        case root : L2_Root => root.nodes
        case other          => ListBuffer(other)
      }
  }
}
