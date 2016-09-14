package exastencils.base.ir

import exastencils.prettyprinting._

trait IR_Access extends IR_Expression {
  // TODO: def name : String
}

/// IR_VariableAccess

object IR_VariableAccess {
  def apply(name : String) = new IR_VariableAccess(name, None)
  def apply(name : String, datatype : IR_Datatype) = new IR_VariableAccess(name, Some(datatype))
}

case class IR_VariableAccess(var name : String, var innerDatatype : Option[IR_Datatype]) extends IR_Access {
  override def datatype = innerDatatype.getOrElse(IR_RealDatatype) // FIXME
  override def prettyprint(out : PpStream) : Unit = out << name
}
