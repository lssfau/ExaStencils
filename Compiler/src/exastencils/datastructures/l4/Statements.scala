package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.l4._

abstract class Statement
  extends Node with CppPrettyPrintable

case class VariableDeclarationStatement(variable : Variable, expression : Option[Expression] = None)
    extends Statement {
  override def cpp = ""

  override def duplicate = {
    if (expression != None)
      VariableDeclarationStatement(Duplicate(variable), Some(Duplicate(expression.get))).asInstanceOf[this.type]
    else
      VariableDeclarationStatement(Duplicate(variable)).asInstanceOf[this.type]
  }
}
case class AssignmentStatement(identifier : Identifier, expression : Expression)
    extends Statement {
  override def cpp = ""

  override def duplicate = { this.copy(identifier = Duplicate(identifier), expression = Duplicate(expression)).asInstanceOf[this.type] }
}

case class ForStatement(begin : VariableDeclarationStatement, end : Expression, inc : Expression, statements : ListBuffer[Statement])
    extends Statement with CppPrettyPrintable {
  override def cpp = ""

  override def duplicate = { this.copy(begin = Duplicate(begin), end = Duplicate(end), inc = Duplicate(inc), statements = Duplicate(statements)).asInstanceOf[this.type] }
}

//area ~ level ~ order ~ blocksize ~ stmts
case class LoopOverDomainStatement(area : String, level : String, order: Some[String], statements : ListBuffer[Statement])
    extends Statement {
  override def cpp = ""
  override def duplicate = { this.copy(statements = Duplicate(statements)).asInstanceOf[this.type] }
}