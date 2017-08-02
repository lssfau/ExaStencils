package exastencils.operator.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldAccess
import exastencils.grid.l4.L4_VirtualFieldAccess

object L4_OffsetAccesses extends QuietDefaultStrategy("Offset accesses to (virtual) fields") {
  var offset = L4_ConstIndex()

  // TODO: introduce trait to mark nodes with offset

  this += new Transformation("Apply", {
    case field : L4_FieldAccess =>
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
