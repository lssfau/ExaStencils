package exastencils.operator.ir

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir.IR_VirtualFieldAccess

object IR_OffsetAccesses extends QuietDefaultStrategy("Offset accesses to (virtual) fields") {
  var offset = IR_ConstIndex()

  this += new Transformation("Apply", {
    case field : IR_FieldAccess        =>
      field.index += offset
      field
    case field : IR_VirtualFieldAccess =>
      field.index += offset
      field
  })
}
