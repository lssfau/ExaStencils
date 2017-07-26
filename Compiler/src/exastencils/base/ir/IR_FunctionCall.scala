package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.hack.ir.HACK_IR_UndeterminedFunctionReference
import exastencils.prettyprinting.PpStream

/// IR_FunctionCall

object IR_FunctionCall {
  def apply(function : IR_FunctionReference, args : IR_Expression*) = new IR_FunctionCall(function, args.to[ListBuffer])

  @deprecated("Used for backwards compatibility - to be removed", "22.09.16")
  def apply(functionName : String, args : IR_Expression*)
  = new IR_FunctionCall(HACK_IR_UndeterminedFunctionReference(functionName, IR_UnitDatatype), args.to[ListBuffer])
  @deprecated("Used for backwards compatibility - to be removed", "22.09.16")
  def apply(functionName : String, args : ListBuffer[IR_Expression])
  = new IR_FunctionCall(HACK_IR_UndeterminedFunctionReference(functionName, IR_UnitDatatype), args)
}

case class IR_FunctionCall(var function : IR_FunctionReference, var arguments : ListBuffer[IR_Expression]) extends IR_Expression {
  def name = function.name
  override def datatype = function.returnType
  override def prettyprint(out : PpStream) = out << function << '(' <<< (arguments, ", ") << ')'
}