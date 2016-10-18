package exastencils.base.l2

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.prettyprinting._

/// L2_FunctionCall

object L2_FunctionCall {
  def apply(identifier : L2_Access, arguments : Option[List[L2_Expression]]) =
    new L2_FunctionCall(identifier, arguments.getOrElse(List()).to[ListBuffer])
}

case class L2_FunctionCall(var identifier : L2_Access, var arguments : ListBuffer[L2_Expression]) extends L2_Expression {
  def prettyprint(out : PpStream) = { out << identifier << " ( " <<< (arguments, ", ") << " )" }
  def progress = L3_FunctionCall(identifier.progress, arguments.map(_.progress))
}
