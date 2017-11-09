package exastencils.base.ir

/// IR_PlainDslFunctionReference

case class IR_PlainDslFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_FunctionReference {
}

/// IR_LeveledDslFunctionReference

case class IR_LeveledDslFunctionReference(var baseName : String, var level : Int, var returnType : IR_Datatype) extends IR_FunctionReference {
  override var name = baseName + '_' + level
}
