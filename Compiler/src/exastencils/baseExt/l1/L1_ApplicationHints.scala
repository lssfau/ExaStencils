package exastencils.baseExt.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l1.L1_Statement
import exastencils.baseExt.l2._
import exastencils.prettyprinting.PpStream
import exastencils.util.l1.L1_GeneralParameter

/// L1_ApplicationHints

object L1_ApplicationHints {
  def apply(hints : List[L1_ApplicationHint]) = new L1_ApplicationHints(hints.to[ListBuffer])
}

case class L1_ApplicationHints(var hints : ListBuffer[L1_ApplicationHint]) extends L1_Statement {
  override def prettyprint(out : PpStream) = out << "ApplicationHint {\n" <<< (hints, "\n") << "\n}"
  override def progress = ProgressLocation(L2_ApplicationHints(hints.map(_.progress)))
}

/// L1_ApplicationHint

abstract class L1_ApplicationHint extends L1_Statement {
  override def progress : L2_ApplicationHint
}

/// L1_ApplicationParameter

case class L1_ApplicationParameter(var name : String, var value : Any) extends L1_ApplicationHint with L1_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def progress = ProgressLocation(L2_ApplicationParameter(name, value))
}
