package exastencils.base.l2

import exastencils.base.l3._
import exastencils.prettyprinting._

/// L2_Statement

trait L2_Statement extends L2_Node with L2_Progressable with PrettyPrintable {
  def progress : L3_Statement
}

/// L2_NullStatement

case object L2_NullStatement extends L2_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = L3_NullStatement
}

/// L2_ExpressionStatement

case class L2_ExpressionStatement(var expression : L2_Expression) extends L2_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression
  override def progress = L3_ExpressionStatement(expression.progress)
}
