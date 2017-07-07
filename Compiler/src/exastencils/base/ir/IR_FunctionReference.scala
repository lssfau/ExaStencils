package exastencils.base.ir

import exastencils.prettyprinting._

/// IR_FunctionReference

trait IR_FunctionReference extends IR_Node with PrettyPrintable {
  var name : String
  def returnType : IR_Datatype

  override def prettyprint(out : PpStream) = out << name
}

/// IR_PlainFunctionReference

trait IR_PlainFunctionReference extends IR_FunctionReference {
  override def prettyprint(out : PpStream) = out << name
}

/// IR_LeveledFunctionReference

trait IR_LeveledFunctionReference extends IR_FunctionReference {
  def baseName : String
  def level : Int
  override def prettyprint(out : PpStream) = out << name
}
