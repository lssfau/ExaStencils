package exastencils.base.l4

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L4_ConstantExpression

trait L4_ConstantExpression extends L4_Expression

/// L4_Number
trait L4_Number extends L4_ConstantExpression {
  def value : AnyVal
}

/// L4_StringLiteral

case class L4_StringLiteral(var value : String) extends L4_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
  override def progress = IR_StringLiteral(value)
}

/// L4_StringConstant

case class L4_StringConstant(var value : String) extends L4_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << '"' << value << '"'
  override def progress = IR_StringConstant(value)
}

/// L4_IntegerConstant
case class L4_IntegerConstant(var v : Long) extends L4_Number {
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
  override def progress = IR_IntegerConstant(value)
}

/// L4_RealConstant
case class L4_RealConstant(var v : Double) extends L4_Number {
  override def prettyprint(out : PpStream) : Unit = {
    out << value // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
  }
  override def value = v
  override def progress = IR_RealConstant(value)
}

/// L4_BooleanConstant

case class L4_BooleanConstant(var value : Boolean) extends L4_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def progress = IR_BooleanConstant(value)
}

// L4_ConvertStringConstantsToLiterals

object L4_ConvertStringConstantsToLiterals extends QuietDefaultStrategy("Convert string constants to literals") {
  this += new Transformation("Convert", {
    case const : L4_StringConstant => L4_StringLiteral(const.value)
  })
}

object L4_ConvertStringLiteralsToConstants extends QuietDefaultStrategy("Convert string literals to constants") {
  this += new Transformation("Convert", {
    case const : L4_StringLiteral => L4_StringConstant(const.value)
  })
}