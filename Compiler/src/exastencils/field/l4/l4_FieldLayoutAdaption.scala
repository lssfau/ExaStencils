package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate

/// L4_AddPaddingToFieldLayouts

object L4_DuplicateFieldLayoutsForFields {
  def apply() = {
    val newFieldLayouts = ListBuffer[L4_FieldLayout]()

    for (field <- L4_FieldCollection.objects) {
      // TODO: check if Duplicate works for knowledge objects
      val fieldLayout = Duplicate(L4_FieldLayoutCollection.getByIdentifier(field.fieldLayout, field.level).get)
      fieldLayout.identifier += "_" + field.identifier
      field.fieldLayout += "_" + field.identifier
      newFieldLayouts += fieldLayout
    }

    // update collection
    L4_FieldLayoutCollection.objects = newFieldLayouts
  }
}