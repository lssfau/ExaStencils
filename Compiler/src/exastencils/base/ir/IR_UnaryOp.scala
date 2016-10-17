package exastencils.base.ir

import exastencils.prettyprinting._

object IR_UnaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  type UnaryOperators = Value

  val Negative = Value("-")
  val Not = Value("!")

  val AddressOf = Value("&")

  def createExpression(op : String, exp : IR_Expression) : IR_Expression = createExpression(withName(op), exp)
  def createExpression(op : Value, exp : IR_Expression) : IR_Expression = op match {
    case Negative => IR_Negative(exp)
    case Not      => IR_Negation(exp)
  }
}

/// arithmetic operations

case class IR_Negative(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
}

/// logical operations

case class IR_Negation(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
}

/// increment and decrement operations

case class IR_PreDecrement(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(--" << left << ')'
}

case class IR_PostDecrement(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "--)"
}

case class IR_PreIncrement(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(++" << left << ')'
}

case class IR_PostIncrement(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "++)"
}

/// other operations

case class IR_AddressOf(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(&" << left << ')'
}
