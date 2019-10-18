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

package exastencils.base.ir

import scala.collection.mutable.ListBuffer

/// IR_Root

object IR_Root {
  def apply(nodes : IR_Node*) = new IR_Root(nodes.to[ListBuffer])
}

case class IR_Root(var nodes : ListBuffer[IR_Node]) extends IR_Node {

  def +=(n : IR_Node*) : IR_Root = {
    nodes ++= n
    this
  }

  // resolve nested root nodes
  def flatten() : Unit = {
    while (nodes.exists(_.isInstanceOf[IR_Root]))
      nodes = nodes.flatMap {
        case root : IR_Root => root.nodes
        case other          => ListBuffer(other)
      }
  }
}
