package exastencils.base.l1

import exastencils.base.l2._

/// L1_Access

trait L1_Access extends L1_Expression {
  def name : String
  override def progress : L2_Access
}
