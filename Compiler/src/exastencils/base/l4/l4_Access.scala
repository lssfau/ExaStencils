package exastencils.base.l4

import exastencils.base.ir._
import exastencils.prettyprinting._

trait L4_Access extends L4_Expression {
  def name : String
  override def progress : IR_Access
}

/// L4_VariableAccess

object L4_VariableAccess {
  def apply(name : String) = new L4_VariableAccess(name, None)
  def apply(name : String, datatype : L4_Datatype) = new L4_VariableAccess(name, Some(datatype))
}

case class L4_VariableAccess(var name : String, var datatype : Option[L4_Datatype]) extends L4_Access {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def progress = IR_VariableAccess(name, L4_ProgressOption(datatype)(_.progress))
}
