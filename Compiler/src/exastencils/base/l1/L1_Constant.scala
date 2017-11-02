package exastencils.base.l1

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L1_ConstantExpression

trait L1_ConstantExpression extends L1_Expression

/// L1_Number
trait L1_Number extends L1_ConstantExpression {
  def value : AnyVal
}

/// L1_StringLiteral

case class L1_StringLiteral(var value : String) extends L1_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
  override def progress = L2_StringLiteral(value)
}

/// L1_StringConstant

case class L1_StringConstant(var value : String) extends L1_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << '"' << value << '"'
  override def progress = L2_StringConstant(value)
}

/// L1_IntegerConstant
case class L1_IntegerConstant(var v : Long) extends L1_Number {
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
  override def progress = L2_IntegerConstant(value)
}

/// L1_RealConstant
case class L1_RealConstant(var v : Double) extends L1_Number {
  override def prettyprint(out : PpStream) : Unit = {
    out << value // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
  }
  override def value = v
  override def progress = L2_RealConstant(value)
}

/// L1_BooleanConstant

case class L1_BooleanConstant(var value : Boolean) extends L1_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def progress = L2_BooleanConstant(value)
}

// L1_ConvertStringConstantsToLiterals

object L1_ConvertStringConstantsToLiterals extends QuietDefaultStrategy("Convert string constants to literals") {
  this += new Transformation("Convert", {
    case const : L1_StringConstant => L1_StringLiteral(const.value)
  })
}

object L1_ConvertStringLiteralsToConstants extends QuietDefaultStrategy("Convert string literals to constants") {
  this += new Transformation("Convert", {
    case const : L1_StringLiteral => L1_StringConstant(const.value)
  })
}
