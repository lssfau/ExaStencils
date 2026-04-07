package exastencils.waLBerla.ir.field

import exastencils.fieldlike.ir.IR_FieldLikeLayoutCollection
import exastencils.fieldlike.ir.IR_FieldLikeLayoutCollections

/// IR_WaLBerlaFieldLayoutCollection

object IR_WaLBerlaFieldLayoutCollection extends IR_FieldLikeLayoutCollection {

  IR_FieldLikeLayoutCollections.register(this)
  exastencils.core.Duplicate.registerConstant(this)
}
