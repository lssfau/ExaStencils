package exastencils.field.ir

import exastencils.config.Platform

/// IR_AddPaddingToFieldLayouts

object IR_AddPaddingToFieldLayouts {
  def apply() = {
    for (fieldLayout <- IR_FieldLayoutCollection.objects) {
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
