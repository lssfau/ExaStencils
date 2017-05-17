package exastencils.base.l2

import exastencils.base.l3._
import exastencils.prettyprinting._

/// L2_Access

trait L2_Access extends L2_Expression {
  def name : String
  override def progress : L3_Access
}

/// L2_VariableAccess

case class L2_VariableAccess(var name : String, /*TODO: level */ var datatype : L2_Datatype) extends L2_Access {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def progress = L3_VariableAccess(name, datatype.progress)
}
