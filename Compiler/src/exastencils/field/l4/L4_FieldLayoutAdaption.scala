package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate

/// L4_AddPaddingToFieldLayouts

object L4_DuplicateFieldLayoutsForFields {
  def apply() = {
    val newFieldLayouts = ListBuffer[L4_FieldLayout]()

    for (field <- L4_FieldCollection.objects) {
      val fieldLayout = Duplicate(field.fieldLayout)
      fieldLayout.identifier += "_" + field.identifier
      field.fieldLayout = fieldLayout
      newFieldLayouts += fieldLayout
    }

    // update collection
    L4_FieldLayoutCollection.objects = newFieldLayouts
  }
}