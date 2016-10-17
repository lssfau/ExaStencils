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
    case Negative => IR_NegativeExpression(exp)
    case Not      => IR_NegationExpression(exp)
  }
}

/// arithmetic operations

case class IR_NegativeExpression(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
}

/// logical operations

case class IR_NegationExpression(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
}

/// increment and decrement operations

case class IR_PreDecrementExpression(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(--" << left << ')'
}

case class IR_PostDecrementExpression(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "--)"
}

case class IR_PreIncrementExpression(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(++" << left << ')'
}

case class IR_PostIncrementExpression(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "++)"
}

/// other operations

case class IR_AddressofExpression(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(&" << left << ')'
}
