package exastencils.base.ir

import exastencils.prettyprinting._

trait IR_Statement extends IR_Node with PrettyPrintable

case object IR_NullStatement extends IR_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
}

case class IR_ExpressionStatement(var expression : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression.prettyprint << ';'
}
