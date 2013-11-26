package exastencils.datastructures

trait AbstractDatatype extends Node with CppPrettyPrintable
case object IntegerDatatype extends AbstractDatatype {
  def cpp : String = { return "int" }
}
case object StringDatatype extends Node with AbstractDatatype {
  def cpp : String = { return "std::string" }
}

case class AbstractVariable(var Name : String, var Type : AbstractDatatype) extends Node with CppPrettyPrintable {
  def cpp : String = {
    return ""
  }
}

abstract class AbstractStatement extends Node with CppPrettyPrintable
case class AbstractVariableDeclarationStatement(var Variable : AbstractVariable, var Expression : Option[AbstractExpression] = None) extends AbstractStatement {
  def cpp : String = { return "" }
}
case class AbstractAssignmentStatement(var Variable : AbstractVariable, var Expression : AbstractExpression) extends AbstractStatement {
  def cpp : String = { return "" }
}

trait AbstractExpression extends Node with CppPrettyPrintable
case class AbstractBooleanExpression(var Left : AbstractExpression, var Operator : String, var Right : AbstractExpression) extends AbstractExpression {
  def cpp : String = { return "" }
}
case class AbstractConstantExpression(var Value : Any) extends AbstractExpression {
  def cpp : String = { return "" }
}

case class AbstractForLoop(var Begin : AbstractVariableDeclarationStatement, var End : AbstractExpression, var Inc : AbstractExpression)
    extends Node with CppPrettyPrintable {
  def cpp : String = {
    return ""
  }
}

