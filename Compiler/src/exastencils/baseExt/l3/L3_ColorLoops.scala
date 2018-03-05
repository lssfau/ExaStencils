package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_ColorLoops
import exastencils.prettyprinting.PpStream

/// L3_ColorLoops

case class L3_ColorLoops(var colorExps : ListBuffer[L3_Modulo], var stmts : ListBuffer[L3_Statement]) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << "color with {\n" <<< (colorExps, ",\n") << ",\n" <<< (stmts, "\n") << "\n}"
  override def progress = ProgressLocation(L4_ColorLoops(colorExps.map(_.progress), stmts.map(_.progress)))
}
