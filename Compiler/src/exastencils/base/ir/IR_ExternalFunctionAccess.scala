package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// IR_ExternalFunctionAccess

object IR_ExternalFunctionAccess {
  def printf = new IR_ExternalFunctionAccess("printf", IR_UnitDatatype)

  def apply(name : String) = new IR_ExternalFunctionAccess(name, IR_UnknownDatatype)
}

case class IR_ExternalFunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_FunctionAccess {
  override def prettyprint(out : PpStream) = out << name
}
