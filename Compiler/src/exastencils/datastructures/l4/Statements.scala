package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._

abstract class Statement
  extends Node with CppPrettyPrintable

case class VariableDeclarationStatement(variable : Variable, expression : Option[Expression] = None)
    extends Statement {
  def cpp : String = { return "" }

  override def duplicate = {
    if (expression != None)
      VariableDeclarationStatement(Duplicate(variable), Some(Duplicate(expression.get))).asInstanceOf[this.type]
    else
      VariableDeclarationStatement(Duplicate(variable)).asInstanceOf[this.type]
  }
}
case class AssignmentStatement(variable : Variable, expression : Expression)
    extends Statement {
  def cpp = { "" }

  override def duplicate = { this.copy(variable = Duplicate(variable), expression = Duplicate(expression)).asInstanceOf[this.type] }
}

case class ForStatement(begin : VariableDeclarationStatement, end : Expression, inc : Expression)
    extends Statement with CppPrettyPrintable {
  def cpp = { "" }

  override def duplicate = { this.copy(begin = Duplicate(begin), end = Duplicate(end), inc = Duplicate(inc)).asInstanceOf[this.type] }
}
