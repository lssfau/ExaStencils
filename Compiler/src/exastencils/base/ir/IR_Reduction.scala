package exastencils.base.ir

/// IR_Reduction

// FIXME: op as BinOp
case class IR_Reduction(var op : String, var target : IR_VariableAccess) extends IR_Node
