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

package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.waLBerla.l4.field._

/// L4_DuplicateFieldLayoutsForFields

// TODO: Use FieldLikeCollections instead or FieldCollection

object L4_DuplicateFieldLayoutsForFields {
  def apply() = {
    val newFieldLayouts = ListBuffer[L4_FieldLayout]()
    val newWaLBerlaFieldLayouts = ListBuffer[L4_WaLBerlaFieldLayout]()

    for (field <- L4_FieldCollection.objects) {
      val fieldLayout = field.fieldLayout.createDuplicate()
      fieldLayout.name += "_" + field.name
      field.fieldLayout = fieldLayout
      newFieldLayouts += fieldLayout
    }

    for (field <- L4_WaLBerlaFieldCollection.objects) {
      val fieldLayout = field.fieldLayout.createDuplicate()
      fieldLayout.name += "_" + field.name
      field.fieldLayout = fieldLayout
      newWaLBerlaFieldLayouts += fieldLayout
    }

    // update collection
    L4_FieldLayoutCollection.objects = newFieldLayouts
    L4_WaLBerlaFieldLayoutCollection.objects = newWaLBerlaFieldLayouts
  }
}
