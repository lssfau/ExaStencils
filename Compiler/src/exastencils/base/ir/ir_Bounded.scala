package exastencils.base.ir

import exastencils.prettyprinting.PpStream

case class IR_BoundedScalar(var min : Long, var max : Long, var expr : IR_Expression) extends IR_Expression {
  override def datatype = expr.datatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = IR_BoundedScalar(" << expr << ')'

  def expandSpecial() = expr
}
