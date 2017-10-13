package exastencils.base.l3

import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_Statement

trait L3_Statement extends L3_Node with L3_Progressable with PrettyPrintable {
  def progress : L4_Statement
}

/// L3_StatementWrapper

// exists to provide convenient way to wrap for applying transformations
case class L3_StatementWrapper(stmt : L3_Statement) extends L3_Node

/// L3_NullStatement

case object L3_NullStatement extends L3_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = L4_NullStatement
}

/// L3_ExpressionStatement

case class L3_ExpressionStatement(var expression : L3_Expression) extends L3_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression
  override def progress = L4_ExpressionStatement(expression.progress)
}
