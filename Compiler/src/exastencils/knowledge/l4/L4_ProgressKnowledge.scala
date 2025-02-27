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

package exastencils.knowledge.l4

import exastencils.fieldlike.l4.L4_FieldLikeCollections
import exastencils.fieldlike.l4.L4_FieldLikeLayoutCollections
import exastencils.interfacing.l4.L4_ExternalFieldCollection
import exastencils.operator.l4._

object L4_ProgressKnowledge {
  def apply() = {
    // may require:
    //L4_DomainCollection.progress()

    // may require:
    for (layoutCollection <- L4_FieldLikeLayoutCollections.collections)
      layoutCollection.progress()

    // may require: Domain, FieldLayout
    for (fieldCollection <- L4_FieldLikeCollections.collections)
      fieldCollection.progress()

    // may require: FieldLayout, Field
    L4_ExternalFieldCollection.progress()

    // may require: Field for non-linear stencils
    L4_StencilCollection.progress()

    // may require: Stencil(Template) and Field
    L4_StencilFieldCollection.progress()
  }
}
