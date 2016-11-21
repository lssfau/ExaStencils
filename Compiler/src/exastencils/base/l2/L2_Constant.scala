package exastencils.base.l2

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L2_ConstantExpression

trait L2_ConstantExpression extends L2_Expression

/// L2_Number
trait L2_Number extends L2_ConstantExpression {
  def value : AnyVal
}

/// L2_StringLiteral

case class L2_StringLiteral(var value : String) extends L2_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
  override def progress = L3_StringLiteral(value)
}

/// L2_StringConstant

case class L2_StringConstant(var value : String) extends L2_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << '"' << value << '"'
  override def progress = L3_StringConstant(value)
}

/// L2_IntegerConstant
case class L2_IntegerConstant(var v : Long) extends L2_Number {
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
  override def progress = L3_IntegerConstant(value)
}

/// L2_RealConstant
case class L2_RealConstant(var v : Double) extends L2_Number {
  override def prettyprint(out : PpStream) : Unit = {
    out << value // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
  }
  override def value = v
  override def progress = L3_RealConstant(value)
}

/// L2_BooleanConstant

case class L2_BooleanConstant(var value : Boolean) extends L2_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def progress = L3_BooleanConstant(value)
}

// L2_ConvertStringConstantsToLiterals

object L2_ConvertStringConstantsToLiterals extends QuietDefaultStrategy("Convert string constants to literals") {
  this += new Transformation("Convert", {
    case const : L2_StringConstant => L2_StringLiteral(const.value)
  })
}

object L2_ConvertStringLiteralsToConstants extends QuietDefaultStrategy("Convert string literals to constants") {
  this += new Transformation("Convert", {
    case const : L2_StringLiteral => L2_StringConstant(const.value)
  })
}