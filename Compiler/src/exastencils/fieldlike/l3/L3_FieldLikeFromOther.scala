package exastencils.fieldlike.l3

import exastencils.base.l3.L3_Access
import exastencils.base.l3.L3_LevelSpecification
import exastencils.fieldlike.l4.L4_FieldLike
import exastencils.logger.Logger

/// L3_FieldLikeFromOther

trait L3_FieldLikeFromOther[L3_Type <: L3_FieldLike[L4_Type], L4_Type <: L4_FieldLike[_, _]] extends L3_FieldLikeDecl[L3_Type, L4_Type] {
  def name : String
  def levels : Option[L3_LevelSpecification]
  def src : L3_Access

  override def progress = Logger.error(s"Trying to progress l3 field declaration for field $name; this is not supported")
}
