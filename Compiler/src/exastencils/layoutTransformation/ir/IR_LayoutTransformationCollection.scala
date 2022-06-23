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

package exastencils.layoutTransformation.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Node
import exastencils.core.ObjectWithState
import exastencils.core.StateManager

object IR_LayoutTransformationCollection extends ObjectWithState {

  // buffer looked up reference to reduce execution time
  var selfRef : Option[IR_LayoutTransformationCollection] = None

  override def clear() = { selfRef = None }

  // looks itself up starting from the current root
  def getOpt = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[IR_LayoutTransformationCollection]()
    selfRef
  }
}

case class IR_LayoutTransformationCollection(var trafoStmts : ListBuffer[IR_LayoutTransformStatement]) extends IR_Node
