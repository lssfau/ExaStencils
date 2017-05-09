package exastencils.base.l2

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.prettyprinting._

/// L2_FunctionCall

object L2_FunctionCall {
  def apply(identifier : L2_Access, arguments : Option[List[L2_Expression]]) =
    new L2_FunctionCall(identifier, arguments.getOrElse(List()).to[ListBuffer])

  @deprecated("Used for backwards compatibility - to be removed", "22.09.16")
  def apply(functionName : String, args : L2_Expression*)
  = new L2_FunctionCall(L2_UserFunctionAccess(functionName, L2_UnitDatatype), args.to[ListBuffer])
  @deprecated("Used for backwards compatibility - to be removed", "22.09.16")
  def apply(functionName : String, args : ListBuffer[L2_Expression])
  = new L2_FunctionCall(L2_UserFunctionAccess(functionName, L2_UnitDatatype), args)
}

case class L2_FunctionCall(var function : L2_Access, var arguments : ListBuffer[L2_Expression]) extends L2_Expression {
  def name = function.name
  def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }
  def progress = L3_FunctionCall(function.progress, arguments.map(_.progress))
}
