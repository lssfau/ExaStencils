package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_Statement
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l3.L3_GeneralParameter

/// L3_ApplicationHints

object L3_ApplicationHints {
  def apply(hints : List[L3_ApplicationHint]) = new L3_ApplicationHints(hints.to[ListBuffer])
}

case class L3_ApplicationHints(var hints : ListBuffer[L3_ApplicationHint]) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << "ApplicationHint {\n" <<< (hints, "\n") << "\n}"
  override def progress = Logger.error(s"Trying to progress L3 application hints; this is not supported")
}

/// L3_ApplicationHint

abstract class L3_ApplicationHint extends L3_Statement {
  def process() : Unit
}

/// L3_ApplicationParameter

case class L3_ApplicationParameter(var name : String, var value : Any) extends L3_ApplicationHint with L3_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def process() = set()
  override def progress = Logger.error(s"Trying to progress L3 application parameter; this is not supported")
}

/// L3_ProcessApplicationHints

object L3_ProcessApplicationHints extends DefaultStrategy("Process application hints") {
  this += Transformation("Process", {
    case coll : L3_ApplicationHints =>
      // handle parameter updates first, then everything else
      val (param, nonParam) = coll.hints.partition(_.isInstanceOf[L3_ApplicationParameter])
      param.foreach(_.process())
      nonParam.foreach(_.process())

      None // consume statements
  })
}
