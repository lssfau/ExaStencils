package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_ColorLoops
import exastencils.prettyprinting.PpStream

/// L3_ColorLoops

object L3_ColorLoops {
  def apply(colors : List[L3_Expression], stmts : List[L3_Statement]) = new L3_ColorLoops(colors.to[ListBuffer], stmts.to[ListBuffer])
}

case class L3_ColorLoops(var colors : ListBuffer[L3_Expression], var stmts : ListBuffer[L3_Statement]) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << "color with {\n" <<< (colors, ",\n") << ",\n" <<< (stmts, "\n") << "\n}"
  override def progress = ProgressLocation(L4_ColorLoops(colors.map(_.progress), stmts.map(_.progress)))
}
