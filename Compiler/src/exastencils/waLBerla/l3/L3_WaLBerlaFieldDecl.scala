package exastencils.waLBerla.l3

import exastencils.fieldlike.l3.L3_FieldLikeDecl
import exastencils.logger.Logger


/// L3_WaLBerlaFieldDecl

abstract class L3_WaLBerlaFieldDecl extends L3_FieldLikeDecl[L3_WaLBerlaField] {
  override def progress = Logger.error(s"Trying to progress l3 field declaration for field $name; this is not supported")
  def addToKnowledge() : Unit
}
