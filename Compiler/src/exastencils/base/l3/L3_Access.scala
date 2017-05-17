package exastencils.base.l3

import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_Access

trait L3_Access extends L3_Expression {
  def name : String
  override def progress : L4_Access
}

/// L3_VariableAccess

case class L3_VariableAccess(var name : String, /*TODO: level */ var datatype : L3_Datatype) extends L3_Access {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def progress = L4_VariableAccess(name, datatype.progress)
}
