package exastencils.hack.ir

import exastencils.base.ir._

/// HACK_IR_UndeterminedFunctionAccess

case class HACK_IR_UndeterminedFunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_FunctionAccess
