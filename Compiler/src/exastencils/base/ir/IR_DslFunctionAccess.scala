package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// IR_PlainDslFunctionAccess

case class IR_PlainDslFunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_FunctionAccess {
}

/// IR_LeveledDslFunctionAccess

case class IR_LeveledDslFunctionAccess(var name : String, var level : Int, var datatype : IR_Datatype) extends IR_FunctionAccess {
  override def prettyprint(out : PpStream) = out << name << '_' << level
}
