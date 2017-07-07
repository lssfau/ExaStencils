package exastencils.base.l3

import exastencils.base.l4._

/// L3_InternalFunctionReference

object L3_InternalFunctionReference {
  def floord = L3_PlainInternalFunctionReference("floord", L3_RealDatatype/* TODO: check data type */)
}

trait L3_InternalFunctionReference extends L3_FunctionReference

/// L3_PlainInternalFunctionReference

case class L3_PlainInternalFunctionReference(var name : String, var returnType : L3_Datatype) extends L3_InternalFunctionReference with L3_PlainFunctionReference {
  override def progress = L4_PlainInternalFunctionReference(name, returnType.progress)
}

/// L3_LeveledInternalFunctionReference

case class L3_LeveledInternalFunctionReference(var name : String, var level : Int, var returnType : L3_Datatype) extends L3_InternalFunctionReference with L3_LeveledFunctionReference {
  override def progress = L4_LeveledInternalFunctionReference(name, level, returnType.progress)
}
