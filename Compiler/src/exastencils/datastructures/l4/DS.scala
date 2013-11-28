package exastencils.datastructures.l4

import exastencils.datastructures._

trait AbstractDatatype extends CppPrettyPrintable
case object IntegerDatatype extends AbstractDatatype {
  def cpp : String = { return "int" }
}
case object StringDatatype extends AbstractDatatype {
  def cpp : String = { return "std::string" }
}

case class AbstractVariable(var Name : String, var Type : AbstractDatatype)
    extends Node with CppPrettyPrintable {
  def cpp : String = {
    return ""
  }

  override def duplicate = { this.copy().asInstanceOf[this.type] }
}

abstract class AbstractStatement
  extends Node with CppPrettyPrintable

case class AbstractVariableDeclarationStatement(var Variable : AbstractVariable, var Expression : Option[AbstractExpression] = None)
    extends AbstractStatement {
  def cpp : String = { return "" }

  override def duplicate = {
    if (Expression != None)
      AbstractVariableDeclarationStatement(Duplicate(Variable), Some(Duplicate(Expression.get))).asInstanceOf[this.type]
    else
      AbstractVariableDeclarationStatement(Duplicate(Variable)).asInstanceOf[this.type]
  }
}
case class AbstractAssignmentStatement(var Variable : AbstractVariable, var Expression : AbstractExpression)
    extends AbstractStatement {
  def cpp : String = { return "" }

  override def duplicate = { this.copy(Variable = Duplicate(Variable), Expression = Duplicate(Expression)).asInstanceOf[this.type] }
}

trait AbstractExpression
    extends Node with CppPrettyPrintable {

  override def duplicate : this.type
}

case class AbstractBooleanExpression(var Left : AbstractExpression, var Operator : String, var Right : AbstractExpression)
    extends AbstractExpression {
  def cpp : String = { return "" }

  override def duplicate = { this.copy(Left = Duplicate(Left), Right = Duplicate(Right)).asInstanceOf[this.type] }
}
case class AbstractConstantExpression(var Value : Any) extends AbstractExpression {
  def cpp : String = { return "" }

  override def duplicate = { this.copy().asInstanceOf[this.type] }
}

case class AbstractForLoop(var Begin : AbstractVariableDeclarationStatement, var End : AbstractExpression, var Inc : AbstractExpression)
    extends Node with CppPrettyPrintable {
  def cpp : String = {
    return ""
  }

  override def duplicate = { this.copy(Begin = Duplicate(Begin), End = Duplicate(End), Inc = Duplicate(Inc)).asInstanceOf[this.type] }
}

