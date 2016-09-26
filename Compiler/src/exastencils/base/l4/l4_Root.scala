package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Root
import exastencils.logger.Logger
import exastencils.multiGrid.MultiGridFunctions
import exastencils.prettyprinting._

object L4_Root {
  def apply(node : L4_Node) = new L4_Root(ListBuffer(node))
  def apply(nodes : List[L4_Node]) = new L4_Root(nodes.to[ListBuffer])
}

case class L4_Root(var nodes : ListBuffer[L4_Node]) extends L4_Node with L4_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = {
    // TODO: print knowledge collections
    nodes.foreach {
      case p : PrettyPrintable => out << p << '\n'
      case _                   =>
    }
  }

  override def progress : IR_Root = {
    var newRoot = IR_Root()

    val functions = MultiGridFunctions()

    nodes.foreach {
      case fct : L4_Function      => functions.functions += fct.progress
      case node : L4_Progressable => newRoot += node.progress
      case node                   => Logger.warn("Found unprogressable L4 node " + node)
    }

    newRoot += functions // FIXME: think about how to manage (MG/other) functions

    newRoot
  }

  def +=(n : L4_Node*) = nodes ++= n

  // resolve root nodes in nodes
  def flatten() = {
    nodes.foreach {
      case root : L4_Root =>
        nodes ++= root.nodes
        nodes -= root
      case _              =>
    }
  }
}
