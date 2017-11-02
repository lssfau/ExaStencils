package exastencils.base.l1

import exastencils.base.l2._

/// L1_InternalFunctionReference

object L1_InternalFunctionReference {
  def floord = L1_PlainInternalFunctionReference("floord", L1_RealDatatype/* TODO: check data type */)
}

trait L1_InternalFunctionReference extends L1_FunctionReference

/// L1_PlainInternalFunctionReference

case class L1_PlainInternalFunctionReference(var name : String, var returnType : L1_Datatype) extends L1_InternalFunctionReference with L1_PlainFunctionReference {
  override def progress = L2_PlainInternalFunctionReference(name, returnType.progress)
}

/// L1_LeveledInternalFunctionReference

case class L1_LeveledInternalFunctionReference(var name : String, var level : Int, var returnType : L1_Datatype) extends L1_InternalFunctionReference with L1_LeveledFunctionReference {
  override def progress = L2_LeveledInternalFunctionReference(name, level, returnType.progress)
}
