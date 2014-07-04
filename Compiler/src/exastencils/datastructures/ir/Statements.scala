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
  def this(body : Statement) = this(ListBuffer(body))

  override def cpp : String = {
    ("{\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}")
  }
}

case class StatementBlock(var body : ListBuffer[Statement]) extends Statement {
  def cpp : String = {
    (body.map(stat => stat.cpp).mkString("\n"))
  }
}

case class VariableDeclarationStatement(var dataType : Datatype, var name : String, var expression : Option[Expression] = None) extends Statement {
  // interface to the old way to define VariableDeclarationStatements
  def this(variable : VariableAccess, expression : Option[Expression]) = this(variable.dType.get, variable.name, expression)
  def this(variable : VariableAccess) = this(variable.dType.get, variable.name, None)

  override def cpp = dataType.resolveUnderlyingDatatype.cpp + " " + name + dataType.resolvePostscript + (if (expression.isDefined) s" = ${expression.get.cpp};" else ";")
  def cpp_onlyDeclaration = { VariableDeclarationStatement(dataType, name, None).cpp }
}

case class DefineStatement(var name : Expression, var value : Option[Expression] = None) extends Statement {
  override def cpp = {
    s"#define ${name.cpp}" + (if (value.isDefined) s"  ${value.get.cpp}" else "")
  }
}

case class CommentStatement(var comment : String) extends Statement {
  override def cpp = {
    "/* " + comment + " */"
  }
}

case class AssignmentStatement(var dest : Expression, var src : Expression, var op : Expression = "=") extends Statement {
  override def cpp : String = {
    (s"${dest.cpp} ${op.cpp} ${src.cpp};")
  }
}

case class ForLoopStatement(var begin : Statement, var end : Expression, var inc : Statement, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement {
  def this(begin : Statement, end : Expression, inc : Statement, body : Statement, reduction : Option[Reduction]) = this(begin, end, inc, ListBuffer(body), reduction)
  def this(begin : Statement, end : Expression, inc : Statement, body : Statement) = this(begin, end, inc, ListBuffer(body))

  override def cpp : String = {

    val sb : StringBuilder = new StringBuilder()
    sb ++= "for (" ++= begin.cpp() += ' '; end.cppsb(sb); sb ++= "; " ++= inc.cpp()
    if (sb.last == ';')
      sb.deleteCharAt(sb.length - 1)
    sb ++= ") {\n"
    for (stmt <- body)
      sb ++= stmt.cpp() += '\n'
    sb += '}'

    return sb.toString()
  }
}

case class ConditionStatement(var condition : Expression, var trueBody : ListBuffer[Statement], var falseBody : ListBuffer[Statement]) extends Statement {
  def this(condition : Expression, trueBody : ListBuffer[Statement]) = this(condition, trueBody, ListBuffer[Statement]())
  def this(condition : Expression, trueBranch : Statement) = this(condition, ListBuffer(trueBranch))

  def this(condition : Expression, trueBranch : Statement, falseBranch : Statement) = this(condition, ListBuffer(trueBranch), ListBuffer(falseBranch))
  def this(condition : Expression, trueBody : ListBuffer[Statement], falseBranch : Statement) = this(condition, trueBody, ListBuffer(falseBranch))
  def this(condition : Expression, trueBranch : Statement, falseBody : ListBuffer[Statement]) = this(condition, ListBuffer(trueBranch), falseBody)

  def cpp : String = {
    (s"if (${condition.cpp}) {\n"
      + trueBody.map(stat => stat.cpp).mkString("\n")
      + s"\n}"
      + (if (falseBody.length > 0)
        s" else {\n"
        + falseBody.map(stat => stat.cpp).mkString("\n")
        + s"\n}"
      else
        ""))
  }
}

case class CaseStatement(var toMatch : Expression, var body : ListBuffer[Statement]) extends Statement {
  def this(toMatch : Expression, body : Statement) = this(toMatch, ListBuffer[Statement](body))

  override def cpp : String = {
    (s"case (${toMatch.cpp}): {\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n} break;")
  }
}

case class SwitchStatement(var what : Expression, var body : ListBuffer[CaseStatement]) extends Statement {
  def this(what : Expression, body : CaseStatement) = this(what, ListBuffer[CaseStatement](body))

  override def cpp : String = {
    (s"switch (${what.cpp}) {\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}")
  }
}

case class ReturnStatement(expr : Expression) extends Statement {
  override def cpp : String = {
    return s"return ${expr.cpp};\n"
  }
}

abstract class AbstractFunctionStatement() extends Statement {
  def cpp_decl : String
}

case class FunctionStatement(var returntype : Datatype, var name : Expression, var parameters : ListBuffer[VariableAccess], var body : ListBuffer[Statement]) extends AbstractFunctionStatement {
  def this(returntype : Datatype, name : Expression, parameters : ListBuffer[VariableAccess], body : Statement) = this(returntype, name, parameters, ListBuffer[Statement](body))
  def this(returntype : Datatype, name : Expression, parameters : VariableAccess, body : ListBuffer[Statement]) = this(returntype, name, ListBuffer[VariableAccess](parameters), body)

  override def cpp : String = { // FIXME: add specialized node for parameter specification with own PP
    (s"${returntype.cpp} ${name.cpp}(" + parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ") {\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}")
  }

  override def cpp_decl : String = {
    s"${returntype.cpp} ${name.cpp}(" + parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n"
  }
}

// FIXME: add ClassStatement, AbstractClassStatement, PrettyPrinter, etc
