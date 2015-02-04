package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.knowledge._
import exastencils.prettyprinting._

abstract class Statement
  extends Node with PrettyPrintable

case class ExpressionStatement(var expression : Expression) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression.prettyprint << ';'
}

case object NullStatement extends Statement {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) : Unit = out << ';'
}

case class Scope(var body : ListBuffer[Statement]) extends Statement {
  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = {
    out << "{\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class VariableDeclarationStatement(var dataType : Datatype, var name : String, var expression : Option[Expression] = None) extends Statement {
  // interface to the old way to define VariableDeclarationStatements
  def this(variable : VariableAccess, expression : Option[Expression]) = this(variable.dType.get, variable.name, expression)
  def this(variable : VariableAccess) = this(variable.dType.get, variable.name, None)

  override def prettyprint(out : PpStream) : Unit = {
    out << dataType.resolveUnderlyingDatatype << ' ' << name << dataType.resolvePostscript
    if (expression.isDefined)
      out << " = " << expression.get
    out << ';'
  }

  def prettyprint_onlyDeclaration() : String = VariableDeclarationStatement(dataType, name, None).prettyprint()
}

case class FreeStatement(var pointer : Expression) extends Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "delete[] " << pointer << ";"
  }
}

case class DefineStatement(var name : Expression, var value : Option[Expression] = None) extends Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "#define " << name
    if (value.isDefined)
      out << ' ' << value.get
  }
}

case class CommentStatement(var comment : String) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "/* " << comment << " */"
}

case class AssignmentStatement(var dest : Expression, var src : Expression, var op : String = "=") extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << dest << ' ' << op << ' ' << src << ';'
}

case class WhileLoopStatement(var comparison : Expression, var body : ListBuffer[Statement]) extends Statement {
  def this(comparison : Expression, body : Statement) = this(comparison, ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = {
    out << "while (" << comparison << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class ForLoopStatement(var begin : Statement, var end : Expression, var inc : Statement, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement {
  def this(begin : Statement, end : Expression, inc : Statement, body : Statement, reduction : Option[Reduction]) = this(begin, end, inc, ListBuffer(body), reduction)
  def this(begin : Statement, end : Expression, inc : Statement, body : Statement) = this(begin, end, inc, ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = {
    // BEGIN AMAZING HACK as workaround for IBM XL compiler    
    var realEnd = end.prettyprint
    if (realEnd.size > 2)
      realEnd = realEnd.substring(1, realEnd.size - 1)
    out << "for (" << begin << ' ' << realEnd << "; " << inc
    // END HACK
    //out << "for (" << begin << ' ' << end << "; " << inc
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

  def prettyprint(out : PpStream) : Unit = {
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

  override def prettyprint(out : PpStream) : Unit = {
    out << "case " << toMatch << ": {\n"
    out <<< (body, "\n") << '\n'
    out << "} break;"
  }
}

case class SwitchStatement(var what : Expression, var body : ListBuffer[CaseStatement]) extends Statement {
  def this(what : Expression, body : CaseStatement) = this(what, ListBuffer[CaseStatement](body))

  override def prettyprint(out : PpStream) : Unit = {
    out << "switch (" << what << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class ReturnStatement(var expr : Option[Expression]) extends Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
    out << ';' << '\n'
  }
}

case class BreakStatement() extends Statement {
  override def prettyprint(out : PpStream) = {
    out << "break;\n"
  }
}

abstract class AbstractFunctionStatement() extends Statement {
  def prettyprint_decl() : String
}

case class FunctionStatement(var returntype : Datatype, var name : String, var parameters : ListBuffer[VariableAccess], var body : ListBuffer[Statement]) extends AbstractFunctionStatement {
  def this(returntype : Datatype, name : String, parameters : ListBuffer[VariableAccess], body : Statement) = this(returntype, name, parameters, ListBuffer[Statement](body))
  def this(returntype : Datatype, name : String, parameters : VariableAccess, body : ListBuffer[Statement]) = this(returntype, name, ListBuffer[VariableAccess](parameters), body)

  override def prettyprint(out : PpStream) : Unit = { // FIXME: add specialized node for parameter specification with own PP
    out << returntype << ' ' << name << ' ' << '('
    if (!parameters.isEmpty) {
      for (param <- parameters)
        out << param.dType.get << ' ' << param.name << ", "
      out.removeLast(2)
    }
    out << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def prettyprint_decl() : String = {
    s"${returntype.prettyprint} $name (" + parameters.map(param => s"${param.dType.get.prettyprint} ${param.name}").mkString(", ") + ");\n"
  }
}

//////////////////////////// SIMD Statements \\\\\\\\\\\\\\\\\\\\\\\\\\\\

case class SIMD_StoreStatement(var mem : Expression, var value : Expression, var aligned : Boolean) extends Statement {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << (if (aligned) "_mm_store_pd" else "_mm_storeu_pd")
      case "AVX" | "AVX2" => out << (if (aligned) "_mm256_store_pd" else "_mm256_storeu_pd")
      case "QPX"          => out << (if (aligned) "vec_sta" else "NOT VALID ; unaligned store for QPX: ")
    }
    out << '(' << mem << ", "
    if (Knowledge.simd_instructionSet == "QPX")
      out << "0, "
    out << value << ");"
  }
}

case class SIMD_HorizontalAddStatement(var dest : Expression, var src : Expression) extends Statement {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3" =>
        out << "{\n"
        out << " __m128d _v = " << src << ";\n"
        out << dest << " += _mm_cvtsd_f64(_mm_hadd_pd(_v,_v));\n"
        out << '}'

      case "AVX" | "AVX2" =>
        out << "{\n"
        out << " __m256d _v = " << src << ";\n"
        out << " __m256d _h = _mm256_hadd_pd(_v,_v);\n"
        out << dest << " += _mm_cvtsd_f64(_mm_add_pd(_mm256_extractf128_pd(_h,1), _mm256_castpd256_pd128(_h)));\n"
        out << '}'

      case "QPX" =>
        HorizontalPrinterHelper.prettyprint(out, dest, src, "add", "+=")
    }
  }
}

case class SIMD_HorizontalMulStatement(var dest : Expression, var src : Expression) extends Statement {
  override def prettyprint(out : PpStream) : Unit = {
    HorizontalPrinterHelper.prettyprint(out, dest, src, "mul", "*=")
  }
}

case class SIMD_HorizontalMinStatement(var dest : Expression, var src : Expression) extends Statement {
  override def prettyprint(out : PpStream) : Unit = {
    if (Knowledge.simd_instructionSet == "QPX")
      out << "NOT VALID ; vec_min not available on BG/Q"
    else
      HorizontalPrinterHelper.prettyprint(out, dest, src, "min", "=", "std::min")
  }
}

case class SIMD_HorizontalMaxStatement(var dest : Expression, var src : Expression) extends Statement {
  override def prettyprint(out : PpStream) : Unit = {
    if (Knowledge.simd_instructionSet == "QPX")
      out << "NOT VALID ; vec_max not available on BG/Q"
    else
      HorizontalPrinterHelper.prettyprint(out, dest, src, "max", "=", "std::max")
  }
}

private object HorizontalPrinterHelper {
  def prettyprint(out : PpStream, dest : Expression, src : Expression, redName : String, assOp : String, redFunc : String = null) : Unit = {
    out << "{\n"
    Knowledge.simd_instructionSet match {
      case "SSE3" =>
        out << " __m128d _v = " << src << ";\n"
        out << " double _r = _mm_cvtsd_f64(_mm_" << redName << "_pd(_v, _mm_shuffle_pd(_v,_v,1)));\n"

      case "AVX" | "AVX2" =>
        out << " __m256d _v = " << src << ";\n"
        out << " __m128d _w = _mm_" << redName << "_pd(_mm256_extractf128_pd(_v,1), _mm256_castpd256_pd128(_v));\n"
        out << " double _r = _mm_cvtsd_f64(_mm_" << redName << "_pd(_w, _mm_permute_pd(_w,1)));\n"

      case "QPX" =>
        out << " vector4double _v = " << src << ";\n"
        out << " _v = vec_" << redName << "(_v, vec_sldw(_v, _v, 2));\n"
        out << " double _r = vec_extract(vec_" << redName << "(_v, vec_sldw(_v, _v, 1)), 0);\n"
    }
    out << dest << ' ' << assOp
    if (redFunc != null)
      out << ' ' << redFunc << '(' << dest << ",_r);\n"
    else
      out << "_r;\n"
    out << '}'
  }
}
