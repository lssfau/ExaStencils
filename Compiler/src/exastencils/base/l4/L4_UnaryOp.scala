package exastencils.base.l4

import exastencils.base.ir._
import exastencils.prettyprinting._

/// L4_UnaryOperators

object L4_UnaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  type UnaryOperators = Value
  val Negative = Value("-")
  val Not = Value("!")

  def createExpression(op : String, exp : L4_Expression) : L4_Expression = createExpression(withName(op), exp)
  def createExpression(op : Value, exp : L4_Expression) : L4_Expression = op match {
    case Negative => L4_Negative(exp)
    case Not      => L4_Negation(exp)
  }
}

/// arithmetic operations

case class L4_Negative(var left : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
  override def progress = IR_Negative(left.progress)
}

/// logical operations

case class L4_Negation(var left : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
  override def progress = IR_Negation(left.progress)
}
