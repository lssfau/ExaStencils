package exastencils.base.ir

import exastencils.knowledge.Knowledge
import exastencils.prettyprinting.PpStream

trait IR_ConstantExpression extends IR_Expression

trait IR_Number extends IR_ConstantExpression {
  def value : AnyVal
}

/// strings

case class IR_StringLiteral(var value : String) extends IR_Expression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
}

case class IR_StringConstant(var value : String) extends IR_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '"' << value << '"'
}

/// numbers

case class IR_IntegerConstant(var v : Long) extends IR_Number {
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
}

case class IR_RealConstant(var v : Double) extends IR_Number {
  override def prettyprint(out : PpStream) : Unit = {
    // FIXME: set single/double locally
    if (Knowledge.useDblPrecision)
      out << IR_DoubleConstant(v)
    else
      out << IR_FloatConstant(v)
  }
  override def value = v
}

case class IR_FloatConstant(var v : Double) extends IR_Number {
  override def prettyprint(out : PpStream) : Unit = out << value << "f"
  override def value = v
}

case class IR_DoubleConstant(var v : Double) extends IR_Number {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def value = v
}

/// other

case class IR_BooleanConstant(var value : Boolean) extends IR_Expression {
  override def prettyprint(out : PpStream) : Unit = out << value
}
