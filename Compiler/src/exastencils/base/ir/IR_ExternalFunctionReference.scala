package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// IR_ExternalFunctionReference

object IR_ExternalFunctionReference {
  def printf = new IR_ExternalFunctionReference("printf", IR_UnitDatatype)

  def apply(name : String) = new IR_ExternalFunctionReference(name, IR_UnknownDatatype)
}

case class IR_ExternalFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_FunctionReference {
  override def prettyprint(out : PpStream) = out << name
}
