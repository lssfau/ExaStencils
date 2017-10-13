package exastencils.hack.ir

import exastencils.base.ir._

/// HACK_IR_UndeterminedFunctionReference

case class HACK_IR_UndeterminedFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_FunctionReference
