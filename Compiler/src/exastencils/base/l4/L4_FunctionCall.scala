package exastencils.base.l4

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_FunctionCall

object L4_FunctionCall {
  def apply(function : L4_Access, arguments : Option[List[L4_Expression]]) =
    new L4_FunctionCall(function, arguments.getOrElse(List()).to[ListBuffer])

  def apply(function : L4_Access, args : L4_Expression*) =
    new L4_FunctionCall(function, args.to[ListBuffer])
}

case class L4_FunctionCall(var function : L4_Access, var arguments : ListBuffer[L4_Expression]) extends L4_Expression {
  override def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }

  override def progress : IR_FunctionCall = {
    function match {
      case access : L4_FunctionAccess   =>
        IR_FunctionCall(access.progress, arguments.map(s => s.progress))
      case access : L4_UnresolvedAccess =>
        Logger.warn("Found function call without resolved access " + access.name)
        // FIXME: access.name
        IR_FunctionCall(access.name, arguments.map(s => s.progress))
    }
  }

  def name = function.name
}
