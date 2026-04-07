package exastencils.waLBerla.l4.field

import exastencils.fieldlike.l4.L4_FieldLikeDecl
import exastencils.logger.Logger
import exastencils.waLBerla.ir.field.IR_WaLBerlaField

abstract class L4_WaLBerlaFieldDecl extends L4_FieldLikeDecl[L4_WaLBerlaField, IR_WaLBerlaField] {
  override def progress = Logger.error(s"Trying to progress l4 field declaration for field $name; this is not supported")
}