package exastencils.base.ir

case class IR_Reduction(var op : String, var target : IR_VariableAccess) extends IR_Node
