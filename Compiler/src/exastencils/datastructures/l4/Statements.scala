package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.l4._

abstract class Statement extends Node

case class VariableDeclarationStatement(var variable : Variable, var expression : Option[Expression] = None)
    extends Statement {
  override def duplicate = {
    if (expression != None)
      VariableDeclarationStatement(Duplicate(variable), Some(Duplicate(expression.get))).asInstanceOf[this.type]
    else
      VariableDeclarationStatement(Duplicate(variable)).asInstanceOf[this.type]
  }
}

case class AssignmentStatement(var identifier : Identifier, var expression : Expression)
    extends Statement {
  override def duplicate = { this.copy(identifier = Duplicate(identifier), expression = Duplicate(expression)).asInstanceOf[this.type] }
}

case class ForStatement(var begin : VariableDeclarationStatement, end : Expression, inc : Expression, var statements : List[Statement])
    extends Statement {
  override def duplicate = { this.copy(begin = Duplicate(begin), end = Duplicate(end), inc = Duplicate(inc), statements = Duplicate(statements)).asInstanceOf[this.type] }
}

case class LoopOverDomainStatement(area : String, level : String, order : Option[String], blocksize : Any, var statements : List[Statement])
    extends Statement {
  override def duplicate = { this.copy(statements = Duplicate(statements)).asInstanceOf[this.type] }
}

case class FunctionStatement(name : String, var returntype : Datatype , var arguments : List[Variable], var statements : List[Statement])
    extends Statement {
  override def duplicate = { this.copy(returntype = Duplicate(returntype), arguments = Duplicate(arguments), statements = Duplicate(statements)).asInstanceOf[this.type] }
}


