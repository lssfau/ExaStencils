package exastencils.baseExt.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l2.L2_Statement
import exastencils.baseExt.l3._
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_GeneralParameter

/// L2_ApplicationHints

object L2_ApplicationHints {
  def apply(hints : List[L2_ApplicationHint]) = new L2_ApplicationHints(hints.to[ListBuffer])
}

case class L2_ApplicationHints(var hints : ListBuffer[L2_ApplicationHint]) extends L2_Statement {
  override def prettyprint(out : PpStream) = out << "ApplicationHint {\n" <<< (hints, "\n") << "\n}"
  override def progress = ProgressLocation(L3_ApplicationHints(hints.map(_.progress)))
}

/// L2_ApplicationHint

abstract class L2_ApplicationHint extends L2_Statement {
  override def progress : L3_ApplicationHint
}

/// L2_ApplicationParameter

case class L2_ApplicationParameter(var name : String, var value : Any) extends L2_ApplicationHint with L2_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def progress = ProgressLocation(L3_ApplicationParameter(name, value))
}
