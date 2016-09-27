package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// IR_FunctionAccess

trait IR_FunctionAccess extends IR_Access {
  // name is read/write
  var name : String
  def datatype : IR_Datatype

  override def prettyprint(out : PpStream) = out << name
}

/// IR_UserFunctionAccess

case class IR_UserFunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_FunctionAccess
