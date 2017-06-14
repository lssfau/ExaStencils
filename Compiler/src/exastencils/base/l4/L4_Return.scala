package exastencils.base.l4

import exastencils.base.ir._
import exastencils.prettyprinting._

/// L4_Return

case class L4_Return(var expr : Option[L4_Expression]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
  }

  override def progress = IR_Return(L4_ProgressOption(expr)(_.progress))
}
