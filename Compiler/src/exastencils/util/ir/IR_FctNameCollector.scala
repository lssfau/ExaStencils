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

import scala.collection.mutable.Stack

import exastencils.base.ir.IR_FunctionLike
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.logger._

class IR_FctNameCollector extends Collector {
  private val nameStack = new Stack[String]

  override def enter(node : Node) : Unit = {
    node match {
      case fct : IR_FunctionLike => nameStack.push(fct.name)
      case _                     =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case loop : IR_FunctionLike => nameStack.pop
      case _                      =>
    }
  }

  override def reset() : Unit = {
    nameStack.clear
  }

  def inFunction : Boolean = nameStack.nonEmpty

  def getCurrentName : String = {
    if (nameStack.isEmpty) {
      Logger.dbg("Trying to access level outside of a valid scope")
      ""
    } else {
      nameStack.head
    }
  }
}
