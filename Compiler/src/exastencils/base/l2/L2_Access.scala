package exastencils.base.l2

import exastencils.base.l3._

trait L2_Access extends L2_Expression {
  def name : String
  override def progress : L3_Access
}