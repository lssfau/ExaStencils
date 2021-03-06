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

package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_Root
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.knowledge.l4.L4_PrintKnowledgeDecl
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_Root

object L4_Root {
  def apply() = new L4_Root(ListBuffer())
  def apply(node : L4_Node) = new L4_Root(ListBuffer(node))
  def apply(nodes : List[L4_Node]) = new L4_Root(nodes.to[ListBuffer])
}

case class L4_Root(var nodes : ListBuffer[L4_Node]) extends L4_Node with L4_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = {
    L4_PrintKnowledgeDecl.apply(out)

    nodes.foreach {
      case p : PrettyPrintable => out << p << "\n\n"
      case other               => Logger.warn(s"Trying to print unsupported L4 node $other")
    }
  }

  override def progress : IR_Root = ProgressLocation {
    var newRoot = IR_Root()

    val functions = IR_UserFunctions()

    nodes.foreach {
      case fct : L4_Function      => functions += fct.progress
      case node : L4_Progressable => newRoot += node.progress
      case node                   => Logger.warn(s"Trying to progress unsupported L4 node $node")
    }

    newRoot += functions

    newRoot
  }

  // resolve nested root nodes
  def flatten() = {
    while (nodes.exists(_.isInstanceOf[L4_Root]))
      nodes = nodes.flatMap {
        case root : L4_Root => root.nodes
        case other          => ListBuffer(other)
      }
  }
}
