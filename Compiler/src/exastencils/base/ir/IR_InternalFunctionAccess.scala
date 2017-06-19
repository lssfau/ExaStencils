package exastencils.base.ir

/// IR_InternalFunctionAccess

trait IR_InternalFunctionAccess

/// IR_PlainInternalFunctionAccess

case class IR_PlainInternalFunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_InternalFunctionAccess with IR_PlainFunctionAccess {
}

/// IR_LeveledInternalFunctionAccess

case class IR_LeveledInternalFunctionAccess(var name : String, var level : Int, var datatype : IR_Datatype) extends IR_InternalFunctionAccess with IR_LeveledFunctionAccess {
}
