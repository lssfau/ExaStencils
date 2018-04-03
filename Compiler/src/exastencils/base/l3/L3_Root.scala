package exastencils.base.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Root
import exastencils.knowledge.l3.L3_PrintKnowledgeDecl
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L3_Root

object L3_Root {
  def apply() = new L3_Root(ListBuffer())
  def apply(node : L3_Node) = new L3_Root(ListBuffer(node))
  def apply(nodes : List[L3_Node]) = new L3_Root(nodes.to[ListBuffer])
}

case class L3_Root(var nodes : ListBuffer[L3_Node]) extends L3_Node with L3_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = {
    L3_PrintKnowledgeDecl.apply(out)

    nodes.foreach {
      case p : PrettyPrintable => out << p << "\n\n"
      case other               => Logger.warn(s"Trying to print unsupported L3 node $other")
    }
  }

  override def progress = ProgressLocation {
    val (progressable, invalid) = nodes.partition(_.isInstanceOf[L3_Progressable])
    invalid.foreach(node => Logger.warn(s"Trying to progress unsupported L3 node $node"))

    L4_Root(progressable.map(_.asInstanceOf[L3_Progressable].progress))
  }

  // resolve nested root nodes
  def flatten() = {
    while (nodes.exists(_.isInstanceOf[L3_Root]))
      nodes = nodes.flatMap {
        case root : L3_Root => root.nodes
        case other          => ListBuffer(other)
      }
  }
}
