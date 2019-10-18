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

package exastencils.util.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.l3.L3_FieldAccess
import exastencils.grid.l3.L3_VirtualFieldAccess

/// L3_CollectFieldAccesses

object L3_CollectFieldAccesses extends QuietDefaultStrategy("Collect field accesses") {
  var fieldAccesses : ListBuffer[L3_FieldAccess] = ListBuffer()
  var vFieldAccesses : ListBuffer[L3_VirtualFieldAccess] = ListBuffer()

  override def apply(node : Option[Node] = None) = {
    fieldAccesses.clear
    vFieldAccesses.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    fieldAccesses.clear
    vFieldAccesses.clear
    super.applyStandalone(node)
  }

  def applyWrappedStandalone(node : L3_Node) = {
    applyStandalone(L3_Root(node))
  }

  this += new Transformation("Collect", {
    case fieldAccess : L3_FieldAccess =>
      fieldAccesses += fieldAccess
      fieldAccess

    case fieldAccess : L3_VirtualFieldAccess =>
      vFieldAccesses += fieldAccess
      fieldAccess
  })
}
