package exastencils.base.ir

import exastencils.prettyprinting._

/// IR_Statement

trait IR_Statement extends IR_Node with PrettyPrintable

/// IR_ScopedStatement

// used as a marker to simplify unnecessary nesting of scopes
trait IR_ScopedStatement extends IR_Statement

/// IR_StatementWrapper

// exists to provide convenient way to wrap for applying transformations
case class IR_StatementWrapper(stmt : IR_Statement) extends IR_Node

/// IR_NullStatement

case object IR_NullStatement extends IR_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
}

/// IR_ExpressionStatement

case class IR_ExpressionStatement(var expression : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression << ';'
}
