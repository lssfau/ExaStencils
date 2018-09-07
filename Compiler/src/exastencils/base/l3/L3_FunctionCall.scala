package exastencils.base.l3

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_FunctionCall

object L3_FunctionCall {
  def apply(function : L3_FunctionReference, args : L3_Expression*) =
    new L3_FunctionCall(function, args.to[ListBuffer])
}

case class L3_FunctionCall(var function : L3_FunctionReference, var arguments : ListBuffer[L3_Expression]) extends L3_Expression {
  def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }
  override def progress = ProgressLocation(L4_FunctionCall(function.progress, arguments.map(_.progress)))
  def name = function.name
}
