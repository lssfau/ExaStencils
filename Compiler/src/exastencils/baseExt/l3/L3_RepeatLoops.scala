package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_RepeatLoops
import exastencils.prettyprinting.PpStream

/// L3_RepeatLoops

object L3_RepeatLoops {
  def apply(colors : List[L3_Expression], stmts : List[L3_Statement]) = new L3_RepeatLoops(colors.to[ListBuffer], stmts.to[ListBuffer])
}

case class L3_RepeatLoops(var conditions : ListBuffer[L3_Expression], var stmts : ListBuffer[L3_Statement]) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << "repeat with {\n" <<< (conditions, ",\n") << ",\n" <<< (stmts, "\n") << "\n}"
  override def progress = ProgressLocation(L4_RepeatLoops(conditions.map(_.progress), stmts.map(_.progress)))
}
