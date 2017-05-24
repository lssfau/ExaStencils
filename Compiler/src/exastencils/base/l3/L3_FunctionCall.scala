package exastencils.base.l3

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_FunctionArgument

object L3_FunctionArgument {
  // generate declaration corresponding to given access
  def apply(access : L3_VariableAccess) = new L3_FunctionArgument(access.name, access.datatype)
}

case class L3_FunctionArgument(var name : String, var datatype : L3_Datatype) extends L3_Node with PrettyPrintable with L3_Progressable {
  override def prettyprint(out : PpStream) = out << name << " : " << datatype
  override def progress = L4_FunctionArgument(name, datatype.progress)
}

/// L3_FunctionCall

object L3_FunctionCall {
  def apply(identifier : L3_Access, arguments : Option[List[L3_Expression]]) =
    new L3_FunctionCall(identifier, arguments.getOrElse(List()).to[ListBuffer])

  @deprecated("Used for backwards compatibility - to be removed", "22.09.16")
  def apply(functionName : String, args : L3_Expression*)
  = new L3_FunctionCall(L3_UserFunctionAccess(functionName, L3_UnitDatatype), args.to[ListBuffer])
  @deprecated("Used for backwards compatibility - to be removed", "22.09.16")
  def apply(functionName : String, args : ListBuffer[L3_Expression])
  = new L3_FunctionCall(L3_UserFunctionAccess(functionName, L3_UnitDatatype), args)
}

case class L3_FunctionCall(var function : L3_Access, var arguments : ListBuffer[L3_Expression]) extends L3_Expression {
  def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }
  def progress = L4_FunctionCall(function.progress, arguments.map(_.progress))
  def name = function.name
}
