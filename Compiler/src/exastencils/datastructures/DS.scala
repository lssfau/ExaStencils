package exastencils.datastructures

// FIXME place this functionality directly into node types
object Cpp {
  def apply(node : Node) : String = node match {
    case x : AbstractForLoop                      => "for-loop: " + Cpp(x.Begin) + "; " + Cpp(x.End) + "; " + Cpp(x.Inc)
    case x : AbstractVariableDeclarationStatement => "vardecl: " + Cpp(x.Variable)
    case x : AbstractVariable                     => "var: " + x.Name + " type: " + x.Type
    case _                                        => ""
  }
}

trait AbstractDatatype extends Node with CppPrettyPrintable
case object IntegerDatatype extends AbstractDatatype {
  def cpp : String = { return "int" }
}
case object StringDatatype extends Node with AbstractDatatype {
  def cpp : String = { return "std::string" }
}

case class AbstractVariable(var Name : String, var Type : AbstractDatatype) extends Node with Traversable[Node] with CppPrettyPrintable {
  def foreach[U](f : Node => U) : Unit = {}
  def cpp : String = {
    return ""
  }
}

abstract class AbstractStatement extends Node with Traversable[Node] with CppPrettyPrintable
case class AbstractVariableDeclarationStatement(var Variable : AbstractVariable, var Expression : Option[AbstractExpression] = None) extends AbstractStatement {
  def foreach[U](f : Node => U) : Unit = {
    f(Variable)
    if (Expression != None) f(Expression.get)
  }
  def cpp : String = { return "" }
}
case class AbstractAssignmentStatement(var Variable : AbstractVariable, var Expression : AbstractExpression) extends AbstractStatement {
  def foreach[U](f : Node => U) : Unit = {
    f(Variable)
    f(Expression)
  }
  def cpp : String = { return "" }
}

trait AbstractExpression extends Node with Traversable[Node] with CppPrettyPrintable
case class AbstractBooleanExpression(var Left : AbstractExpression, var Operator : String, var Right : AbstractExpression) extends AbstractExpression {
  def foreach[U](f : Node => U) : Unit = {
    Left.foreach(f)
    Right.foreach(f)
    f(Left)
    f(Right)
  }
  def cpp : String = { return "" }
}
case class AbstractConstantExpression(var Value : Any) extends AbstractExpression {
  def foreach[U](f : Node => U) : Unit = {}
  def cpp : String = { return "" }
}

case class AbstractForLoop(var Begin : AbstractVariableDeclarationStatement, var End : AbstractExpression, var Inc : AbstractExpression)
    extends Node with Traversable[Node] with CppPrettyPrintable with Debuggable {
  def foreach[U](f : Node => U) : Unit = {
    Begin.foreach(f)
    End.foreach(f)
    Inc.foreach(f)
    f(Begin)
    f(End)
    f(Inc)
  }
  def Replace(oldNode : AbstractVariableDeclarationStatement, newNode : AbstractVariableDeclarationStatement) = {
    println("For: Replace()")
    if (Begin == oldNode) Begin = newNode
  }
  def replace(oldNode : AbstractExpression, newNode : AbstractExpression) = {
    if (End == oldNode) End = newNode
    if (Inc == oldNode) Inc = newNode
  }
  def cpp : String = {
    return ""
  }
  def debug : String = {
    return ""
  }
}

