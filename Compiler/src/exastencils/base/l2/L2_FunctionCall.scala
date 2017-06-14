package exastencils.base.l2

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.prettyprinting._

/// L2_FunctionCall

object L2_FunctionCall {
  def apply(function : L2_Access, arguments : Option[List[L2_Expression]]) =
    new L2_FunctionCall(function, arguments.getOrElse(List()).to[ListBuffer])

  def apply(function : L2_Access, args : L2_Expression*) =
    new L2_FunctionCall(function, args.to[ListBuffer])
}

case class L2_FunctionCall(var function : L2_Access, var arguments : ListBuffer[L2_Expression]) extends L2_Expression {
  def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }
  def progress = L3_FunctionCall(function.progress, arguments.map(_.progress))
  def name = function.name
}
