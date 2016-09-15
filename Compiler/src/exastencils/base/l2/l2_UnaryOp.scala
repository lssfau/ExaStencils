package exastencils.base.l2

import exastencils.base.l3._
import exastencils.prettyprinting._

object L2_UnaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  type UnaryOperators = Value
  val Negative = Value("-")
  val Not = Value("!")

  def createExpression(op : String, exp : L2_Expression) : L2_Expression = createExpression(withName(op), exp)
  def createExpression(op : Value, exp : L2_Expression) : L2_Expression = op match {
    case Negative => L2_NegativeExpression(exp)
    case Not      => L2_NegationExpression(exp)
  }
}

/// arithmetic operations

case class L2_NegativeExpression(var left : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
  override def progress = L3_NegativeExpression(left.progress)
}

/// logical operations

case class L2_NegationExpression(var left : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
  override def progress = L3_NegationExpression(left.progress)
}
