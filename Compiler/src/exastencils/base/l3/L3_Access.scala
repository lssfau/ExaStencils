package exastencils.base.l3

import exastencils.base.l4._

/// L3_Access

trait L3_Access extends L3_Expression {
  def name : String
  override def progress : L4_Access
}
