package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_Root extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_Root.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.base.|LAYER_LC|

import scala.collection.mutable.ListBuffer

import exastencils.base.|NEXT_LC|.|NEXT_UC|_Root
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// |LAYER_UC|_Root

object |LAYER_UC|_Root {
  def apply(node : |LAYER_UC|_Node) = new |LAYER_UC|_Root(ListBuffer(node))
  def apply(nodes : List[|LAYER_UC|_Node]) = new |LAYER_UC|_Root(nodes.to[ListBuffer])
}

case class |LAYER_UC|_Root(var nodes : ListBuffer[|LAYER_UC|_Node]) extends |LAYER_UC|_Node with |LAYER_UC|_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = {
    // TODO: print knowledge collections
    nodes.foreach {
      case p : PrettyPrintable => out << p << "\n\n"
      case _                   =>
    }
  }

  override def progress = {
    val progressableNodes = ListBuffer[|LAYER_UC|_Progressable]()
    nodes.foreach {
      case node : |LAYER_UC|_Progressable => progressableNodes += node
      case node                   => Logger.warn("Found un-progressable node " + node)
    }
    |NEXT_UC|_Root(progressableNodes.map(_.progress))
  }
}
"""
  }
}
