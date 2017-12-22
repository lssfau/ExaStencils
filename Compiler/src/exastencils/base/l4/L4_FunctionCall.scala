package exastencils.base.l4

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.prettyprinting._

/// L4_FunctionCall

object L4_FunctionCall {
  def apply(function : L4_FunctionReference, arguments : Option[List[L4_Expression]]) =
    new L4_FunctionCall(function, arguments.getOrElse(List()).to[ListBuffer])

  def apply(function : L4_FunctionReference, args : L4_Expression*) =
    new L4_FunctionCall(function, args.to[ListBuffer])
}

case class L4_FunctionCall(var function : L4_FunctionReference, var arguments : ListBuffer[L4_Expression]) extends L4_Expression {
  def name = function.name
  override def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }
  override def progress = ProgressLocation(IR_FunctionCall(function.progress, arguments.map(s => s.progress)))
}
