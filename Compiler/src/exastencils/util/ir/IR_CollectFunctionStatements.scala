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

import scala.collection.mutable.HashSet

import exastencils.base.ir.IR_Function
import exastencils.core.ObjectWithState
import exastencils.datastructures._

/// IR_CollectFunctionStatements

object IR_CollectFunctionStatements extends DefaultStrategy("Collecting internal function statements") with ObjectWithState {
  var internalFunctions = HashSet[String]()

  override def clear() = internalFunctions.clear()

  override def apply(node : Option[Node] = None) : Unit = {
    clear()
    super.apply(node)
  }

  this += new Transformation("Collecting", {
    case fct : IR_Function =>
      internalFunctions += fct.name
      fct
  }, false)
}
