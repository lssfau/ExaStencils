package exastencils.base.ir

import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

trait IR_ConstantExpression extends IR_Expression

trait IR_Number extends IR_ConstantExpression {
  def value : AnyVal
}

/// strings

case class IR_StringLiteral(var value : String) extends IR_Expression {
  override def datatype = IR_UnknownDatatype
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
}

case class IR_StringConstant(var value : String) extends IR_Expression {
  override def datatype = IR_StringDatatype
  override def prettyprint(out : PpStream) : Unit = out << '"' << value << '"'
}

/// numbers

case class IR_IntegerConstant(var v : Long) extends IR_Number {
  override def datatype = IR_IntegerDatatype
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
}

case class IR_RealConstant(var v : Double) extends IR_Number {
  override def datatype = IR_RealDatatype
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
  override def datatype = IR_FloatDatatype
  override def prettyprint(out : PpStream) : Unit = {
    if (value == Double.PositiveInfinity)
      out << "(1.0f/0.0f)"
    else if (value == Double.NegativeInfinity)
      out << "(-1.0f/0.0f)"
    else if (java.lang.Double.isNaN(value))
      out << "(0.0f/0.0f)"
    else
      out << value << "f"
  }
  override def value = v
}

case class IR_DoubleConstant(var v : Double) extends IR_Number {
  override def datatype = IR_DoubleDatatype
  override def prettyprint(out : PpStream) : Unit = {
    if (value == Double.PositiveInfinity)
      out << "(1.0/0.0)"
    else if (value == Double.NegativeInfinity)
      out << "(-1.0/0.0)"
    else if (java.lang.Double.isNaN(value))
      out << "(0.0/0.0)"
    else
      out << value
  }
  override def value = v
}

/// other

case class IR_BooleanConstant(var value : Boolean) extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << value
}
