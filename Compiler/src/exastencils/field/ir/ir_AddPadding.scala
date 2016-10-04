package exastencils.field.ir

import exastencils.knowledge._

/// IR_AddPaddingToFieldLayouts

object IR_AddPaddingToFieldLayouts {
  def apply() = {
    for (fieldLayout <- IR_FieldLayoutCollection.objects) {
      // shortcut to innermost layout information
      val innerLayout = fieldLayout.layoutsPerDim(0)

      // left padding
      innerLayout.numPadLayersLeft = (Platform.simd_vectorSize - innerLayout.numGhostLayersLeft % Platform.simd_vectorSize) % Platform.simd_vectorSize

      // right padding
      val total = innerLayout.numPadLayersLeft + innerLayout.numGhostLayersLeft + innerLayout.numDupLayersLeft + innerLayout.numInnerLayers + innerLayout.numDupLayersRight + innerLayout.numGhostLayersRight
      innerLayout.numPadLayersRight = (Platform.simd_vectorSize - total % Platform.simd_vectorSize) % Platform.simd_vectorSize

      // update total number of values in the field
      for (layout <- fieldLayout.layoutsPerDim) layout.updateTotal()

      // update reference offset since left padding may have changed
      fieldLayout.updateDefReferenceOffset()
    }
  }
}
