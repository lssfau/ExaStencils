package exastencils.base.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l2.L2_Root
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L1_Root

object L1_Root {
  def apply() = new L1_Root(ListBuffer())
  def apply(node : L1_Node) = new L1_Root(ListBuffer(node))
  def apply(nodes : List[L1_Node]) = new L1_Root(nodes.to[ListBuffer])
}

case class L1_Root(var nodes : ListBuffer[L1_Node]) extends L1_Node with L1_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = {
    // TODO: print knowledge collections

    nodes.foreach {
      case p : PrettyPrintable => out << p << "\n\n"
      case other               => Logger.warn(s"Trying to print unsupported L1 node $other")
    }
  }

  override def progress = ProgressLocation {
    val (progressable, invalid) = nodes.partition(_.isInstanceOf[L1_Progressable])
    invalid.foreach(node => Logger.warn(s"Trying to progress unsupported L1 node $node"))

    L2_Root(progressable.map(_.asInstanceOf[L1_Progressable].progress))
  }

  // resolve nested root nodes
  def flatten() = {
    while (nodes.exists(_.isInstanceOf[L1_Root]))
      nodes = nodes.flatMap {
        case root : L1_Root => root.nodes
        case other          => ListBuffer(other)
      }
  }
}
