package exastencils.base.ir

import exastencils.prettyprinting._

/// IR_Return

object IR_Return {
  def apply() = new IR_Return(None)
  def apply(expr : IR_Expression) = new IR_Return(Option(expr))
}

case class IR_Return(var expr : Option[IR_Expression]) extends IR_Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
    out << ';'
  }
}
