package exastencils.datastructures

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
  override def deepClone = { this.copy() }
}

abstract class AbstractStatement
  extends Node with CppPrettyPrintable

case class AbstractVariableDeclarationStatement(var Variable : AbstractVariable, var Expression : Option[AbstractExpression] = None)
    extends AbstractStatement {
  def cpp : String = { return "" }
  override def deepClone = {
    if (Expression != None)
      AbstractVariableDeclarationStatement(Variable.deepClone, Some(Expression.get.deepClone))
    else
      AbstractVariableDeclarationStatement(Variable.deepClone)
  }
}
case class AbstractAssignmentStatement(var Variable : AbstractVariable, var Expression : AbstractExpression)
    extends AbstractStatement {
  def cpp : String = { return "" }
  override def deepClone = { this.copy(Variable = this.Variable.deepClone, Expression = this.Expression.deepClone) }
}

trait AbstractExpression
  extends Node with CppPrettyPrintable {
  override def deepClone : AbstractExpression
}
  

case class AbstractBooleanExpression(var Left : AbstractExpression, var Operator : String, var Right : AbstractExpression)
    extends AbstractExpression {
  def cpp : String = { return "" }
  override def deepClone : AbstractBooleanExpression = { this.copy(Left = this.Left.deepClone, Right = this.Right.deepClone) }
}
case class AbstractConstantExpression(var Value : Any) extends AbstractExpression {
  def cpp : String = { return "" }
  override def deepClone : AbstractConstantExpression = { this.copy() }
}

case class AbstractForLoop(var Begin : AbstractVariableDeclarationStatement, var End : AbstractExpression, var Inc : AbstractExpression)
    extends Node with CppPrettyPrintable {
  def cpp : String = {
    return ""
  }
  override def deepClone = { this.copy(Begin = this.Begin.deepClone, End = this.End.deepClone, Inc = this.Inc.deepClone) }
}

