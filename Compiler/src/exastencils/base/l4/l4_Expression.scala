package exastencils.base.l4

import exastencils.base.ir._
import exastencils.prettyprinting._

trait L4_Expression extends L4_Node with L4_Progressable with PrettyPrintable {
  def progress : IR_Expression
}

case object L4_NullExpression extends L4_Expression {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = IR_NullExpression
}
