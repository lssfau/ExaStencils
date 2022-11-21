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

package exastencils.field.ir

import exastencils.config.Platform
import exastencils.fieldlike.ir.IR_FieldLikeLayoutCollections

/// IR_AddPaddingToFieldLayouts

object IR_AddPaddingToFieldLayouts {
  def apply() = {
    for (fieldLayout <- IR_FieldLikeLayoutCollections.collections.flatMap(_.objects)) {
      // shortcut to innermost layout information
      val innerLayout = fieldLayout.layoutsPerDim(0)

      val vs = Platform.simd_vectorSize

      // left padding
      innerLayout.numPadLayersLeft = (vs - innerLayout.numGhostLayersLeft % vs) % vs

      // right padding
      val total = innerLayout.numPadLayersLeft + innerLayout.numGhostLayersLeft + innerLayout.numDupLayersLeft + innerLayout.numInnerLayers + innerLayout.numDupLayersRight + innerLayout.numGhostLayersRight
      innerLayout.numPadLayersRight = (vs - total % vs) % vs

      // update total number of values in the field
      for (layout <- fieldLayout.layoutsPerDim) layout.updateTotal()

      // update reference offset since left padding may have changed
      fieldLayout.updateDefReferenceOffset()
    }
  }
}
