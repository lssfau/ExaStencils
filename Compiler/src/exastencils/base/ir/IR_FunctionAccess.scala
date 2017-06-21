package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// L4_FunctionAccess

trait IR_FunctionAccess extends IR_Access {
  var name : String
  def datatype : IR_Datatype

  override def prettyprint(out : PpStream) = out << name
}

/// IR_PlainFunctionAccess

trait IR_PlainFunctionAccess extends IR_FunctionAccess {
  override def prettyprint(out : PpStream) = out << name
}

/// IR_LeveledFunctionAccess

trait IR_LeveledFunctionAccess extends IR_FunctionAccess {
  def baseName : String
  def level : Int
  override def prettyprint(out : PpStream) = out << name
}
