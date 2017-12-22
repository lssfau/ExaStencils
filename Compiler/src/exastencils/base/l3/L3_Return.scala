package exastencils.base.l3

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_Return

case class L3_Return(var expr : Option[L3_Expression]) extends L3_Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
  }

  override def progress = ProgressLocation(L4_Return(L3_ProgressOption(expr)(_.progress)))
}
