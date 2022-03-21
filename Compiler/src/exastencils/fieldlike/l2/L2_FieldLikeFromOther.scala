package exastencils.fieldlike.l2

import exastencils.base.l2.L2_Access
import exastencils.base.l2.L2_LevelSpecification
import exastencils.logger.Logger

/// L2_FieldLikeFromOther

trait L2_FieldLikeFromOther[L2_Type <: L2_FieldLike[_]] extends L2_FieldLikeDecl[L2_Type] {
  def name : String
  def levels : Option[L2_LevelSpecification]
  def src : L2_Access

  override def progress = Logger.error(s"Trying to progress l2 field declaration for field $name; this is not supported")
}
