package exastencils.waLBerla.l2.field

import exastencils.fieldlike.l2.L2_FieldLikeDecl
import exastencils.logger.Logger
import exastencils.waLBerla.l3.field.L3_WaLBerlaField


/// L2_WaLBerlaFieldDecl

abstract class L2_WaLBerlaFieldDecl extends L2_FieldLikeDecl[L2_WaLBerlaField, L3_WaLBerlaField] {
  override def progress = Logger.error(s"Trying to progress l2 field declaration for field $name; this is not supported")
  def addToKnowledge() : Unit
}
