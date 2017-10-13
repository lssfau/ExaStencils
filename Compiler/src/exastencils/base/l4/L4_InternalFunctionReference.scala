package exastencils.base.l4

import exastencils.base.ir._

/// L4_InternalFunctionReference

object L4_InternalFunctionReference {
  def floord = L4_PlainInternalFunctionReference("floord", L4_RealDatatype/* TODO: check data type */)
}

trait L4_InternalFunctionReference extends L4_FunctionReference

/// L4_PlainInternalFunctionReference

case class L4_PlainInternalFunctionReference(var name : String, var returnType : L4_Datatype) extends L4_InternalFunctionReference with L4_PlainFunctionReference {
  override def progress = IR_PlainInternalFunctionReference(name, returnType.progress)
}

/// L4_LeveledInternalFunctionReference

case class L4_LeveledInternalFunctionReference(var name : String, var level : Int, var returnType : L4_Datatype) extends L4_InternalFunctionReference with L4_LeveledFunctionReference {
  override def progress = IR_LeveledInternalFunctionReference(name, level, returnType.progress)
}
