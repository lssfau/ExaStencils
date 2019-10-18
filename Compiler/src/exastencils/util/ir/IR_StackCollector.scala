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

package exastencils.util.ir

import exastencils.base.ir.IR_Node
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger._

class IR_StackCollector extends Collector {
  var stack : List[IR_Node] = Nil

  def isEmpty : Boolean = { stack.isEmpty }
  def head : IR_Node = { stack.head }

  override def enter(node : Node) : Unit = {
    node match {
      case n : IR_Node => stack ::= n
      case _           =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case n : IR_Node =>
        if (head ne n) Logger.error(s"StackCollector mismatch: Cannot leave(): head is not $n") // fatal error is fatal
        stack = stack.tail

      case _ =>
    }
  }

  override def reset() : Unit = {
    stack = Nil
  }
}
