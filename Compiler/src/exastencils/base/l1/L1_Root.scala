//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.base.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l2.L2_Root
import exastencils.knowledge.l1.L1_PrintKnowledgeDecl
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
    L1_PrintKnowledgeDecl.apply(out)

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
