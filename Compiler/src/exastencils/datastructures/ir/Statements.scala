package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.ir._

abstract class Statement
  extends Node with CppPrettyPrintable

case class ExpressionStatement(var expression : Expression) extends Statement {
  override def cpp = expression.cpp
  override def duplicate = { this.copy(expression = Duplicate(expression)).asInstanceOf[this.type] }
}

case class VariableDeclarationStatement(var variable : Variable, var expression : Option[Expression] = None)
    extends Statement {
  override def cpp = ""

  override def duplicate = {
    if (expression != None)
      VariableDeclarationStatement(Duplicate(variable), Some(Duplicate(expression.get))).asInstanceOf[this.type]
    else
      VariableDeclarationStatement(Duplicate(variable)).asInstanceOf[this.type]
  }
}

case class AssignmentStatement(var identifier : Identifier, var expression : Expression)
    extends Statement {
  override def cpp = ""

  override def duplicate = { this.copy(identifier = Duplicate(identifier), expression = Duplicate(expression)).asInstanceOf[this.type] }
}

case class ForStatement(var begin : VariableDeclarationStatement, var end : Expression, var inc : Expression, var statements : List[Statement])
    extends Statement with CppPrettyPrintable {
  override def cpp = ""

  override def duplicate = { this.copy(begin = Duplicate(begin), end = Duplicate(end), inc = Duplicate(inc), statements = Duplicate(statements)).asInstanceOf[this.type] }
}

case class FunctionStatement(var name : String, var returntype : Datatype, var arguments : List[Variable], var statements : List[Statement])
    extends Statement {
  override def cpp = ""
  override def duplicate = { this.copy(returntype = Duplicate(returntype), arguments = Duplicate(arguments), statements = Duplicate(statements)).asInstanceOf[this.type] }
}
