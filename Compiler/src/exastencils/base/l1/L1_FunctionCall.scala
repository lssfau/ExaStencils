package exastencils.base.l1

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._

/// L1_FunctionCall

object L1_FunctionCall {
  def apply(function : L1_FunctionReference, args : L1_Expression*) =
    new L1_FunctionCall(function, args.to[ListBuffer])
}

case class L1_FunctionCall(var function : L1_FunctionReference, var arguments : ListBuffer[L1_Expression]) extends L1_Expression {
  def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }
  override def progress = ProgressLocation(L2_FunctionCall(function.progress, arguments.map(_.progress)))
  def name = function.name
}
