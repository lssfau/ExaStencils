package exastencils.base.l1

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._

/// L1_Statement

trait L1_Statement extends L1_Node with L1_Progressable with PrettyPrintable {
  def progress : L2_Statement
}

/// L1_StatementWrapper

// exists to provide convenient way to wrap for applying transformations
case class L1_StatementWrapper(stmt : L1_Statement) extends L1_Node

/// L1_NullStatement

case object L1_NullStatement extends L1_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = ProgressLocation(L2_NullStatement)
}

/// L1_ExpressionStatement

case class L1_ExpressionStatement(var expression : L1_Expression) extends L1_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression
  override def progress = ProgressLocation(L2_ExpressionStatement(expression.progress))
}
