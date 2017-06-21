package exastencils.base.ir

/// IR_InternalFunctionAccess

object IR_InternalFunctionAccess {
  def floord = IR_PlainInternalFunctionAccess("floord", IR_RealDatatype/* TODO: check data type */)
}

trait IR_InternalFunctionAccess

/// IR_PlainInternalFunctionAccess

case class IR_PlainInternalFunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_InternalFunctionAccess with IR_PlainFunctionAccess {
}

/// IR_LeveledInternalFunctionAccess

case class IR_LeveledInternalFunctionAccess(var baseName : String, var level : Int, var datatype : IR_Datatype) extends IR_InternalFunctionAccess with IR_LeveledFunctionAccess {
  override var name = baseName + '_' + level
}
