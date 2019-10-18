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

package exastencils.core.collectors

import exastencils.datastructures.Node
import exastencils.logger._

class GeneralStackCollector extends Collector {
  var stack : List[Node] = Nil

  def isEmpty : Boolean = { stack.isEmpty }
  def head : Node = { stack.head }

  override def enter(node : Node) : Unit = {
    stack ::= node
  }

  override def leave(node : Node) : Unit = {
    if (head ne node) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $node") // fatal error is fatal
    stack = stack.tail
  }

  override def reset() : Unit = {
    stack = Nil
  }
}
