package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// L4_FunctionAccess

trait IR_FunctionAccess2 extends IR_Access {
  def name : String
  def datatype : IR_Datatype
}

/// IR_PlainFunctionAccess

trait IR_PlainFunctionAccess extends IR_FunctionAccess {
  override def prettyprint(out : PpStream) = out << name
}

/// IR_LeveledFunctionAccess

trait IR_LeveledFunctionAccess extends IR_FunctionAccess {
  def level : Int
  override def prettyprint(out : PpStream) = out << name << '_' << level
}

/// IR_FunctionAccess

trait IR_FunctionAccess extends IR_Access {
  // name is read/write
  var name : String
  def datatype : IR_Datatype

  override def prettyprint(out : PpStream) = out << name
}

/// IR_UserFunctionAccess

case class IR_UserFunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_FunctionAccess
