package exastencils.base.l3

import exastencils.base.l4._
import exastencils.prettyprinting._

object L3_UnaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  type UnaryOperators = Value
  val Negative = Value("-")
  val Not = Value("!")

  def createExpression(op : String, exp : L3_Expression) : L3_Expression = createExpression(withName(op), exp)
  def createExpression(op : Value, exp : L3_Expression) : L3_Expression = op match {
    case Negative => L3_NegativeExpression(exp)
    case Not      => L3_NegationExpression(exp)
  }
}

/// arithmetic operations

case class L3_NegativeExpression(var left : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
  override def progress = L4_NegativeExpression(left.progress)
}

/// logical operations

case class L3_NegationExpression(var left : L3_Expression) extends L3_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
  override def progress = L4_NegationExpression(left.progress)
}
