package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

abstract class Statement
  extends Node with CppPrettyPrintable

case class ExpressionStatement(var expression : Expression) extends Statement {
  override def cpp = expression.cpp + ";"
}

case class NullStatement() extends Statement {
  def cpp : String = ""
}

case class Scope(var body : ListBuffer[Statement]) extends Statement {
  override def cpp : String = {
    ("\n{\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}");
  }
}

case class StatementBlock(var body : ListBuffer[Statement]) extends Statement {
  def cpp : String = {
    (body.map(stat => stat.cpp).mkString("\n"));
  }
}

case class VariableDeclarationStatement(var variable : VariableAccess, var expression : Option[Expression] = None) extends Statement {
  override def cpp = {
    s"${variable.dType.get.cpp} ${variable.name}" + (if (expression.isDefined) s" = ${expression.get.cpp};" else ";");
  }
}

case class DefineStatement(var name : Expression, var value : Option[Expression] = None) extends Statement {
  override def cpp = {
    s"#define ${name.cpp}" + (if (value.isDefined) s"  ${value.get.cpp}" else "");
  }
}

case class AssignmentStatement(var dest : Expression, var src : Expression, var op : Expression = "=") extends Statement {
  override def cpp : String = {
    (s"${dest.cpp} ${op.cpp} ${src.cpp};");
  }
}

case class ForLoopStatement(var begin : Expression, var end : Expression, var inc : Expression, var body : ListBuffer[Statement], var addOMPStatements : String = "") extends Statement {
  def this(begin : Expression, end : Expression, inc : Expression, body : Statement, addOMPStatements : String) = this(begin, end, inc, ListBuffer(body), addOMPStatements);
  def this(begin : Expression, end : Expression, inc : Expression, body : Statement) = this(begin, end, inc, ListBuffer(body));

  override def cpp : String = {
    (s"for (${begin.cpp}; ${end.cpp}; ${inc.cpp})"
      + "\n{\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}");
  }
}

case class ConditionStatement(var condition : Expression, var trueBody : ListBuffer[Statement], var falseBody : ListBuffer[Statement]) extends Statement {
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

case class CaseStatement(var toMatch : Expression, var body : ListBuffer[Statement]) extends Statement {
  def this(toMatch : Expression, body : Statement) = this(toMatch, ListBuffer[Statement](body));

  override def cpp : String = {
    (s"case (${toMatch.cpp}):"
      + "\n{\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n} break;");
  }
}

case class SwitchStatement(var what : Expression, var body : ListBuffer[CaseStatement]) extends Statement {
  def this(what : Expression, body : CaseStatement) = this(what, ListBuffer[CaseStatement](body));

  override def cpp : String = {
    (s"switch (${what.cpp})"
      + "\n{\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}");
  }
}

case class ReturnStatement(expr : Expression) extends Statement {
  override def cpp : String = {
    return s"return ${expr.cpp};\n"
  }
}

abstract class AbstractFunctionStatement() extends Statement

case class FunctionStatement(var returntype : Datatype, var name : String, var parameters : ListBuffer[VariableAccess], var body : ListBuffer[Statement]) extends AbstractFunctionStatement {
  def this(returntype : Datatype, name : String, parameters : ListBuffer[VariableAccess], body : Statement) = this(returntype, name, parameters, ListBuffer[Statement](body));
  def this(returntype : Datatype, name : String, parameters : VariableAccess, body : ListBuffer[Statement]) = this(returntype, name, ListBuffer[VariableAccess](parameters), body);

  def cpp : String = { // FIXME: add specialized node for parameter specification with own PP
    (s"${returntype.cpp} $name(" + parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ")"
      + "\n{\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}")
  }
}

// FIXME: add ClassStatement, AbstractClassStatement, PrettyPrinter, etc
