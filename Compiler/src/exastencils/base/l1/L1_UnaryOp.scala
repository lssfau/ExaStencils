package exastencils.base.l1

import exastencils.base.l2._
import exastencils.prettyprinting._

/// L1_UnaryOperators

object L1_UnaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerConstant(this)

  type UnaryOperators = Value
  val Negative = Value("-")
  val Not = Value("!")

  def createExpression(op : String, exp : L1_Expression) : L1_Expression = createExpression(withName(op), exp)
  def createExpression(op : Value, exp : L1_Expression) : L1_Expression = op match {
    case Negative => L1_Negative(exp)
    case Not      => L1_Negation(exp)
  }
}

/// arithmetic operations

case class L1_Negative(var left : L1_Expression) extends L1_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
  override def progress = L2_Negative(left.progress)
}

/// logical operations

case class L1_Negation(var left : L1_Expression) extends L1_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
  override def progress = L2_Negation(left.progress)
}
