package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.knowledge._

abstract class Statement
  extends Node with CppPrettyPrintable

case class ExpressionStatement(var expression : Expression) extends Statement {
  override def cpp(out : CppStream) : Unit = out << expression.cpp << ';'
}

case object NullStatement extends Statement {
  exastencils.core.Duplicate.registerConstant(this)
  def cpp(out : CppStream) : Unit = out << ';'
}

case class Scope(var body : ListBuffer[Statement]) extends Statement {
  def this(body : Statement) = this(ListBuffer(body))

  override def cpp(out : CppStream) : Unit = {
    out << "{\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class VariableDeclarationStatement(var dataType : Datatype, var name : String, var expression : Option[Expression] = None) extends Statement {
  // interface to the old way to define VariableDeclarationStatements
  def this(variable : VariableAccess, expression : Option[Expression]) = this(variable.dType.get, variable.name, expression)
  def this(variable : VariableAccess) = this(variable.dType.get, variable.name, None)

  override def cpp(out : CppStream) : Unit = {
    out << dataType.resolveUnderlyingDatatype << ' ' << name << dataType.resolvePostscript
    if (expression.isDefined)
      out << " = " << expression.get
    out << ';'
  }

  def cpp_onlyDeclaration() : String = VariableDeclarationStatement(dataType, name, None).cpp()
}

case class DefineStatement(var name : Expression, var value : Option[Expression] = None) extends Statement {
  override def cpp(out : CppStream) : Unit = {
    out << "#define " << name
    if (value.isDefined)
      out << ' ' << value.get
  }
}

case class CommentStatement(var comment : String) extends Statement {
  override def cpp(out : CppStream) : Unit = out << "/* " << comment << " */"
}

case class AssignmentStatement(var dest : Expression, var src : Expression, var op : String = "=") extends Statement {
  override def cpp(out : CppStream) : Unit = out << dest << ' ' << op << ' ' << src << ';'
}

case class WhileLoopStatement(var comparison : Expression, var body : ListBuffer[Statement]) extends Statement {
  def this(comparison : Expression, body : Statement) = this(comparison, ListBuffer(body))

  override def cpp(out : CppStream) : Unit = {
    out << "while (" << comparison << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class ForLoopStatement(var begin : Statement, var end : Expression, var inc : Statement, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement {
  def this(begin : Statement, end : Expression, inc : Statement, body : Statement, reduction : Option[Reduction]) = this(begin, end, inc, ListBuffer(body), reduction)
  def this(begin : Statement, end : Expression, inc : Statement, body : Statement) = this(begin, end, inc, ListBuffer(body))

  override def cpp(out : CppStream) : Unit = {
    out << "for (" << begin << ' ' << end << "; " << inc
    if (out.last == ';')
      out.removeLast()
    out << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class ConditionStatement(var condition : Expression, var trueBody : ListBuffer[Statement], var falseBody : ListBuffer[Statement]) extends Statement {
  def this(condition : Expression, trueBody : ListBuffer[Statement]) = this(condition, trueBody, ListBuffer[Statement]())
  def this(condition : Expression, trueBranch : Statement) = this(condition, ListBuffer(trueBranch))

  def this(condition : Expression, trueBranch : Statement, falseBranch : Statement) = this(condition, ListBuffer(trueBranch), ListBuffer(falseBranch))
  def this(condition : Expression, trueBody : ListBuffer[Statement], falseBranch : Statement) = this(condition, trueBody, ListBuffer(falseBranch))
  def this(condition : Expression, trueBranch : Statement, falseBody : ListBuffer[Statement]) = this(condition, ListBuffer(trueBranch), falseBody)

  def cpp(out : CppStream) : Unit = {
    out << "if (" << condition << ") {\n"
    out <<< (trueBody, "\n") << '\n'
    if (!falseBody.isEmpty) {
      out << "} else {\n"
      out <<< (falseBody, "\n")
    }
    out << '}'
  }
}

case class CaseStatement(var toMatch : Expression, var body : ListBuffer[Statement]) extends Statement {
  def this(toMatch : Expression, body : Statement) = this(toMatch, ListBuffer[Statement](body))

  override def cpp(out : CppStream) : Unit = {
    out << "case " << toMatch << ": {\n"
    out <<< (body, "\n") << '\n'
    out << "} break;"
  }
}

case class SwitchStatement(var what : Expression, var body : ListBuffer[CaseStatement]) extends Statement {
  def this(what : Expression, body : CaseStatement) = this(what, ListBuffer[CaseStatement](body))

  override def cpp(out : CppStream) : Unit = {
    out << "switch (" << what << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class ReturnStatement(expr : Expression) extends Statement {
  override def cpp(out : CppStream) : Unit = out << "return " << expr << ';'
}

abstract class AbstractFunctionStatement() extends Statement {
  def cpp_decl() : String
}

case class FunctionStatement(var returntype : Datatype, var name : Expression, var parameters : ListBuffer[VariableAccess], var body : ListBuffer[Statement]) extends AbstractFunctionStatement {
  def this(returntype : Datatype, name : Expression, parameters : ListBuffer[VariableAccess], body : Statement) = this(returntype, name, parameters, ListBuffer[Statement](body))
  def this(returntype : Datatype, name : Expression, parameters : VariableAccess, body : ListBuffer[Statement]) = this(returntype, name, ListBuffer[VariableAccess](parameters), body)

  override def cpp(out : CppStream) : Unit = { // FIXME: add specialized node for parameter specification with own PP
    out << returntype << ' ' << name << '('
    if (!parameters.isEmpty) {
      for (param <- parameters)
        out << param.dType.get << ' ' << param.name << ", "
      out.removeLast(2)
    }
    out << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def cpp_decl() : String = {
    s"${returntype.cpp} ${name.cpp}(" + parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n"
  }
}

// FIXME: add ClassStatement, AbstractClassStatement, PrettyPrinter, etc

//////////////////////////// SIMD Statements \\\\\\\\\\\\\\\\\\\\\\\\\\\\

case class SIMD_StoreStatement(var mem : Expression, var value : Expression, var aligned : Boolean) extends Statement {
  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << (if (aligned) "_mm_store_pd" else "_mm_storeu_pd")
      case "AVX" | "AVX2" => out << (if (aligned) "_mm256_store_pd" else "_mm256_storeu_pd")
    }
    out << '(' << mem << ", " << value << ");"
  }
}

case class SIMD_HorizontalAddStatement(var dest : Expression, var src : Expression, var op : String = "=") extends Statement {
  override def cpp(out : CppStream) : Unit = {
    out << "{\n"
    Knowledge.simd_instructionSet match {
      case "SSE3" =>
        out << " __m128d v = " << src << ";\n"
        out << dest << ' ' << op << " _mm_cvtsd_f64(_mm_hadd_pd(v,v));\n"

      case "AVX" | "AVX2" =>
        out << " __m256d v = " << src << ";\n"
        out << " __m256d h = _mm256_hadd_pd(v,v);\n"
        out << dest << ' ' << op << " _mm_cvtsd_f64(_mm_add_pd(_mm256_extractf128_pd(h,1), _mm256_castpd256_pd128(h)));\n"
    }
    out << '}'
  }
}

case class SIMD_HorizontalMulStatement(var dest : Expression, var src : Expression, var op : String = "=") extends Statement {
  override def cpp(out : CppStream) : Unit = {
    out << "{\n"
    Knowledge.simd_instructionSet match {
      case "SSE3" =>
        out << " __m128d v = " << src << ";\n"
        out << dest << ' ' << op << " _mm_cvtsd_f64(_mm_mul_pd(v, _mm_shuffle_pd(v,v,1)));\n"

      case "AVX" | "AVX2" =>
        out << " __m256d v = " << src << ";\n"
        out << " __m128d w = _mm_mul_pd(_mm256_extractf128_pd(v,1), _mm256_castpd256_pd128(v));\n"
        out << dest << ' ' << op << " _mm_cvtsd_f64(_mm_mul_pd(w, _mm_permute_pd(w,1)));\n"
    }
    out << '}'
  }
}
