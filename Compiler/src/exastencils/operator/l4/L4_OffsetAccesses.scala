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

package exastencils.operator.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.fieldlike.l4.L4_FieldLikeAccess
import exastencils.grid.l4.L4_VirtualFieldAccess

object L4_OffsetAccesses extends QuietDefaultStrategy("Offset accesses to (virtual) fields") {
  var offset = L4_ConstIndex()

  // TODO: introduce trait to mark nodes with offset

  this += new Transformation("Apply", {
    case field : L4_FieldLikeAccess =>
      if (field.offset.isDefined)
        field.offset = Some(field.offset.get + offset)
      else
        field.offset = Some(offset)
      field

    case field : L4_VirtualFieldAccess =>
      field.index += offset
      field
  })
}
