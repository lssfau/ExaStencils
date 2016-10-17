package exastencils.base.l3

import exastencils.base.l4._
import exastencils.prettyprinting._

trait L3_Statement extends L3_Node with L3_Progressable with PrettyPrintable {
  def progress : L4_Statement
}

case object L3_NullStatement extends L3_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = L4_NullStatement
}

case class L3_ExpressionStatement(var expression : L3_Expression) extends L3_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression << '\n'
  override def progress = L4_ExpressionStatement(expression.progress)
}
