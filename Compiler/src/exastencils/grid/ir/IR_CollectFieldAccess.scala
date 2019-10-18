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

package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess

object IR_CollectFieldAccess extends QuietDefaultStrategy("Collect field accesses") {
  var fieldAccesses : ListBuffer[IR_FieldAccess] = ListBuffer()
  var vFieldAccesses : ListBuffer[IR_VirtualFieldAccess] = ListBuffer()

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

  this += new Transformation("Collect", {
    case fieldAccess : IR_FieldAccess        =>
      fieldAccesses += fieldAccess
      fieldAccess
    case fieldAccess : IR_VirtualFieldAccess =>
      vFieldAccesses += fieldAccess
      fieldAccess
  })
}
