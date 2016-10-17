package exastencils.base.l2

import exastencils.base.l3._
import exastencils.prettyprinting._

trait L2_Statement extends L2_Node with L2_Progressable with PrettyPrintable {
  def progress : L3_Statement
}

case object L2_NullStatement extends L2_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = L3_NullStatement
}

case class L2_ExpressionStatement(var expression : L2_Expression) extends L2_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression << '\n'
  override def progress = L3_ExpressionStatement(expression.progress)
}
