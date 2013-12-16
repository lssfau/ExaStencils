package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.primitives.Expandable

abstract class Statement
  extends Node with CppPrettyPrintable with Duplicable

case class ExpressionStatement(var expression : Expression) extends Statement {
  override def duplicate = { this.copy(expression = Duplicate(expression)).asInstanceOf[this.type] }

  override def cpp = expression.cpp
}

case class NullStatement() extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = ""
}

case class Scope(var body : ListBuffer[Statement]) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = {
    ("\n{\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}");
  }
}

case class StatementBlock(var body : ListBuffer[Statement]) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    (body.map(stat => stat.cpp).mkString("\n"));
  }
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

// Changed by Sebastian: I needed a more general version
//case class AssignmentStatement(var identifier : Identifier, var expression : Expression)
//    extends Statement {
//  override def cpp = ""
//
//  override def duplicate = { this.copy(identifier = Duplicate(identifier), expression = Duplicate(expression)).asInstanceOf[this.type] }
//}

case class AssignmentStatement(var dest : Expression, var src : Expression) extends Statement {
  override def duplicate = { this.copy(dest = Duplicate(dest), src = Duplicate(src)).asInstanceOf[this.type] }

  override def cpp : String = {
    (s"${dest.cpp} = ${src.cpp};");
  }
}

case class ForLoopStatement(var begin : Expression /*changed by Sebastian - originally: VariableDeclarationStatement*/ , var end : Expression, var inc : Expression, var body : ListBuffer[Statement]) extends Statement {
  override def duplicate = { this.copy(begin = Duplicate(begin), end = Duplicate(end), inc = Duplicate(inc), body = Duplicate(body)).asInstanceOf[this.type] }

  def this(begin : Expression, end : Expression, inc : Expression, body : Statement) = this(begin, end, inc, ListBuffer[Statement](body));

  override def cpp : String = {
    (s"for (${begin.cpp}; ${end.cpp}; ${inc.cpp})"
      + "\n{\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}");
  }
}

case class ConditionStatement(var condition : Expression, var trueBody : ListBuffer[Statement], var falseBody : ListBuffer[Statement]) extends Statement {
  override def duplicate = { this.copy(condition = Duplicate(condition), trueBody = Duplicate(trueBody), falseBody = Duplicate(falseBody)).asInstanceOf[this.type] }

  def this(condition : Expression, trueBody : ListBuffer[Statement]) = this(condition, trueBody, ListBuffer[Statement]());
  def this(condition : Expression, trueBranch : Statement) = this(condition, ListBuffer(trueBranch));

  def this(condition : Expression, trueBranch : Statement, falseBranch : Statement) = this(condition, ListBuffer(trueBranch), ListBuffer(falseBranch));
  def this(condition : Expression, trueBody : ListBuffer[Statement], falseBranch : Statement) = this(condition, trueBody, ListBuffer(falseBranch));
  def this(condition : Expression, trueBranch : Statement, falseBody : ListBuffer[Statement]) = this(condition, ListBuffer(trueBranch), falseBody);

  def cpp : String = {
    (s"if (${condition.cpp})"
      + "\n{\n"
      + trueBody.map(stat => stat.cpp).mkString("\n")
      + s"\n}"
      + (if (falseBody.length > 0)
        s"\nelse\n{\n"
        + falseBody.map(stat => stat.cpp).mkString("\n")
        + s"\n}";
      else
        ""))
  }
}

abstract class AbstractFunctionStatement() extends Statement

case class FunctionStatement(var returntype : Datatype, var name : String, var parameters : ListBuffer[Variable], var body : ListBuffer[Statement]) extends AbstractFunctionStatement {
  override def duplicate = { this.copy(returntype = Duplicate(returntype), parameters = Duplicate(parameters), body = Duplicate(body)).asInstanceOf[this.type] }

  def cpp : String = { // FIXME: add specialized node for parameter specification with own PP
    (s"${returntype.cpp} $name(" + parameters.map(param => s"${param.datatype.cpp} ${param.name}").mkString(", ") + ")"
      + "\n{\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}")
  }
}

// FIXME: add ClassStatement, AbstractClassStatement, PrettyPrinter, etc
