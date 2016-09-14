package exastencils.base.l4

import exastencils.base.ir._
import exastencils.prettyprinting._

trait L4_ConstantExpression extends L4_Expression

trait L4_Number extends L4_ConstantExpression {
  def value : AnyVal
}

case class L4_StringLiteral(var value : String) extends L4_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
  override def progress = IR_StringLiteral(value)
}

case class L4_StringConstant(var value : String) extends L4_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << '"' << value << '"'
  override def progress = IR_StringConstant(value)
}

case class L4_IntegerConstant(var v : Long) extends L4_Number {
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
  override def progress = IR_IntegerConstant(value)
}

case class L4_RealConstant(var v : Double) extends L4_Number {
  override def prettyprint(out : PpStream) : Unit = {
    out << value // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
  }
  override def value = v
  override def progress = IR_RealConstant(value)
}

case class L4_BooleanConstant(var value : Boolean) extends L4_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def progress = IR_BooleanConstant(value)
}
