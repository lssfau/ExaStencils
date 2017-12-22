package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.prettyprinting._

/// L4_Statement

trait L4_Statement extends L4_Node with L4_Progressable with PrettyPrintable {
  def progress : IR_Statement
}

/// L4_StatementWrapper

// exists to provide convenient way to wrap for applying transformations
case class L4_StatementWrapper(stmt : L4_Statement) extends L4_Node

/// L4_NullStatement

case object L4_NullStatement extends L4_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = ProgressLocation(IR_NullStatement)
}

/// L4_ExpressionStatement

case class L4_ExpressionStatement(var expression : L4_Expression) extends L4_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression
  override def progress = ProgressLocation(IR_ExpressionStatement(expression.progress))
}
