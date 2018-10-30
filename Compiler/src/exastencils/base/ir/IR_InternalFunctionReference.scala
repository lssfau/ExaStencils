package exastencils.base.ir

/// IR_InternalFunctionReference

object IR_InternalFunctionReference {
  def floord = IR_PlainInternalFunctionReference("floord", IR_IntegerDatatype)
}

trait IR_InternalFunctionReference extends IR_FunctionReference

/// IR_PlainInternalFunctionReference

case class IR_PlainInternalFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_InternalFunctionReference with IR_PlainFunctionReference {
}

/// IR_LeveledInternalFunctionReference

case class IR_LeveledInternalFunctionReference(var baseName : String, var level : Int, var returnType : IR_Datatype) extends IR_InternalFunctionReference with IR_LeveledFunctionReference {
  override var name = baseName + '_' + level
}
