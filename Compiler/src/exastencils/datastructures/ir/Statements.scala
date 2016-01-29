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
  override def prettyprint(out : PpStream): Unit = out << ';'
}

case class Scope(var body : ListBuffer[Statement]) extends Statement {
  def this(body : Statement*) = this(body.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << "{\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class VariableDeclarationStatement(var dataType : Datatype, var name : String, var expression : Option[Expression] = None) extends Statement {
  def this(dT : Datatype, n : String, e : Expression) = this(dT, n, Some(e))
  def this(va : VariableAccess) = this(va.dType.get, va.name, None)
  def this(va : VariableAccess, e : Expression) = this(va.dType.get, va.name, Some(e))

  override def prettyprint(out : PpStream) : Unit = {
    dataType match {
      case x : VectorDatatype => {
        out << x << ' ' << name
        if (expression.isDefined) {
          out << "("
          expression.get.asInstanceOf[VectorExpression].prettyprintInner(out)
          out << ")"
        }
      }
      case x : MatrixDatatype => {
        out << x << ' ' << name
        if (expression.isDefined) {
          out << "("
          expression.get.asInstanceOf[MatrixExpression].prettyprintInner(out)
          out << ")"
        }
      }
      case _ => {
        out << dataType.resolveUnderlyingDatatype << ' ' << name << dataType.resolvePostscript
        if (expression.isDefined)
          out << " = " << expression.get
      }
    }

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
  def this(n : Expression, v : Expression) = this(n, Option(v))

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
  override def prettyprint(out : PpStream) : Unit = {
    src match {
      //      case x : VectorExpression => {
      //        out << dest << ' ' << op << ' '
      //        x.prettyprintInner(out)
      //        out << ';'
      //      }
      //      case x : MatrixExpression => {
      //        out << dest << ' ' << op << ' '
      //        x.prettyprintInner(out)
      //        out << ';'
      //      }
      case _ => out << dest << ' ' << op << ' ' << src << ';'
    }
  }
}

case class WhileLoopStatement(var comparison : Expression, var body : ListBuffer[Statement]) extends Statement {
  def this(comparison : Expression, body : Statement*) = this(comparison, body.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << "while (" << comparison << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class ForLoopStatement(var begin : Statement, var end : Expression, var inc : Statement, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement {
  def this(begin : Statement, end : Expression, inc : Statement, reduction : Reduction, body : Statement*) = this(begin, end, inc, body.to[ListBuffer], Option(reduction))
  def this(begin : Statement, end : Expression, inc : Statement, body : Statement*) = this(begin, end, inc, body.to[ListBuffer])

  def maxIterationCount() = {
    if (hasAnnotation("numLoopIterations"))
      getAnnotation("numLoopIterations").get.value.asInstanceOf[Int]
    else
      0 // TODO: warning?
  }

  override def prettyprint(out : PpStream) : Unit = {
    // BEGIN AMAZING HACK as workaround for IBM XL compiler
    var realEnd = end.prettyprint
    if (realEnd.size > 2 && realEnd(0) == '(')
      realEnd = realEnd.substring(1, realEnd.size - 1)
    var realInc = inc.prettyprint
    if (realInc.size > 2 && realInc(0) == '(')
      realInc = realInc.substring(1, realInc.size - 1)
    out << "for (" << begin << ' ' << realEnd << "; " << realInc
    // END HACK
    //out << "for (" << begin << ' ' << end << "; " << inc
    val last = out.last
    if (last == ';' || last == ')') // ')' in case of upper hack removed the ';' instead of the closing bracket
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

  override def prettyprint(out : PpStream): Unit = {
    out << "if (" << condition << ") {\n"
    out <<< (trueBody, "\n") << '\n'
    if (!falseBody.isEmpty) {
      out << "} else {\n"
      out <<< (falseBody, "\n") << '\n'
    }
    out << '}'
  }
}

case class CaseStatement(var toMatch : Expression, var body : ListBuffer[Statement]) extends Statement {
  def this(toMatch : Expression, body : Statement*) = this(toMatch, body.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << "case " << toMatch << ": {\n"
    out <<< (body, "\n") << '\n'
    out << "} break;"
  }
}

case class SwitchStatement(var what : Expression, var body : ListBuffer[CaseStatement]) extends Statement {
  def this(what : Expression, body : CaseStatement*) = this(what, body.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << "switch (" << what << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class ReturnStatement(var expr : Option[Expression] = None) extends Statement {
  def this(expr : Expression) = this(Option(expr))

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

abstract class AbstractFunctionStatement(var isHeaderOnly : Boolean = false) extends Statement {
  def prettyprint_decl() : String
}

case class FunctionStatement(
    var returntype : Datatype,
    var name : String,
    var parameters : ListBuffer[VariableAccess],
    var body : ListBuffer[Statement],
    var allowInlining : Boolean = true,
    var allowFortranInterface : Boolean = true) extends AbstractFunctionStatement {
  def this(returntype : Datatype, name : String, parameters : ListBuffer[VariableAccess], body : Statement) = this(returntype, name, parameters, ListBuffer[Statement](body))
  def this(returntype : Datatype, name : String, parameters : VariableAccess, body : ListBuffer[Statement]) = this(returntype, name, ListBuffer[VariableAccess](parameters), body)

  override def prettyprint(out : PpStream) : Unit = { // FIXME: add specialized node for parameter specification with own PP
    out << returntype << ' ' << name << ' ' << '('
    if (!parameters.isEmpty) {
      for (param <- parameters)
        out << param.printDeclaration << ", "
      out.removeLast(2)
    }
    out << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def prettyprint_decl() : String = {
    s"${returntype.prettyprint} $name (" + parameters.map(param => s"${param.printDeclaration}").mkString(", ") + ");\n"
  }
}

//////////////////////////// SIMD Statements \\\\\\\\\\\\\\\\\\\\\\\\\\\\

case class SIMD_StoreStatement(var mem : Expression, var value : Expression, var aligned : Boolean) extends Statement {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    val alig = if (aligned) "" else "u"
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_store" << alig << "_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_store" << alig << "_p" << prec
      case "QPX"          => out << (if (aligned) "vec_sta" else "NOT VALID ; unaligned store for QPX: ")
      case "NEON"         => out << "vst1q_f32"
    }
    Knowledge.simd_instructionSet match {
      case "QPX" => out << '(' << value << ", 0, " << mem << ");"
      case _     => out << '(' << mem << ", " << value << ");"
    }
  }
}

case class SIMD_HorizontalAddStatement(var dest : Expression, var src : Expression) extends Statement {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3" =>
        out << "{\n"
        if (Knowledge.useDblPrecision) {
          out << " __m128d _v = " << src << ";\n"
          out << dest << " += _mm_cvtsd_f64(_mm_hadd_pd(_v,_v));\n"
        } else {
          out << " __m128 _v = " << src << ";\n"
          out << " __m128 _h = _mm_hadd_ps(_v,_v);\n"
          out << dest << " += _mm_cvtss_f32(_mm_add_ps(_h, _mm_movehdup_ps(_h)));\n"
        }
        out << '}'

      case "AVX" | "AVX2" =>
        out << "{\n"
        if (Knowledge.useDblPrecision) {
          out << " __m256d _v = " << src << ";\n"
          out << " __m256d _h = _mm256_hadd_pd(_v,_v);\n"
          out << dest << " += _mm_cvtsd_f64(_mm_add_pd(_mm256_extractf128_pd(_h,1), _mm256_castpd256_pd128(_h)));\n"
        } else {
          out << " __m256 _v = " << src << ";\n"
          out << " __m256 _h = _mm256_hadd_ps(_v,_v);\n"
          out << " __m128 _i = _mm_add_ps(_mm256_extractf128_ps(_h,1), _mm256_castps256_ps128(_h));\n"
          out << dest << " += _mm_cvtss_f32(_mm_hadd_ps(_i,_i));\n"
        }
        out << '}'

      case "QPX" | "NEON" =>
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
        if (Knowledge.useDblPrecision) {
          out << " __m128d _v = " << src << ";\n"
          out << " double _r = _mm_cvtsd_f64(_mm_" << redName << "_pd(_v, _mm_shuffle_pd(_v,_v,1)));\n"
        } else {
          out << " __m128 _v = " << src << ";\n"
          out << " __m128 _h = _mm_" << redName << "_ps(_v, _mm_shuffle_ps(_v,_v,177)); // abcd -> badc\n"
          out << " float _r = _mm_cvtss_f32(_mm_" << redName << "_ps(_h, _mm_shuffle_ps(_h,_h,10))); // a_b_ -> bbaa\n"
        }

      case "AVX" | "AVX2" =>
        if (Knowledge.useDblPrecision) {
          out << " __m256d _v = " << src << ";\n"
          out << " __m128d _w = _mm_" << redName << "_pd(_mm256_extractf128_pd(_v,1), _mm256_castpd256_pd128(_v));\n"
          out << " double _r = _mm_cvtsd_f64(_mm_" << redName << "_pd(_w, _mm_permute_pd(_w,1)));\n"
        } else {
          out << " __m256 _v = " << src << ";\n"
          out << " __m128 _w = _mm_" << redName << "_ps(_mm256_extractf128_ps(_v,1), _mm256_castps256_ps128(_v));\n"
          out << " __m128 _h = _mm_" << redName << "_ps(_w, _mm_shuffle_ps(_w,_w,177)); // abcd -> badc\n"
          out << " float _r = _mm_cvtss_f32(_mm_" << redName << "_ps(_h, _mm_shuffle_ps(_h,_h,10))); // a_b_ -> bbaa\n"
        }

      case "QPX" =>
        out << " vector4double _v = " << src << ";\n"
        out << " _v = vec_" << redName << "(_v, vec_sldw(_v, _v, 2));\n"
        out << ' ' << RealDatatype << " _r = (" << RealDatatype << ") vec_extract(vec_" << redName << "(_v, vec_sldw(_v, _v, 1)), 0);\n"

      case "NEON" =>
        out << " float32x4_t _v = " << src << ";\n"
        out << " float32x2_t _w = v" << redName << "_f32(vget_high_f32(_v), vget_low_f32(_v));\n"
        out << " float _r = vget_lane_f32(_w,0);\n"
        out << " _r " << assOp
        if (redFunc != null)
          out << ' ' << redFunc << "(_r, vget_lane_f32(_w,1));\n"
        else
          out << " vget_lane_f32(_w,1);\n"
    }
    out << dest << ' ' << assOp
    if (redFunc != null)
      out << ' ' << redFunc << '(' << dest << ",_r);\n"
    else
      out << " _r;\n"
    out << '}'
  }
}

/** Special declaration for a SIMD vector, which is initialized with the values 0, 1, ..., Knowledge.simd_vectorSize-1. */
case class SIMD_IncrementVectorDeclaration(var name : String) extends Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << SIMD_RealDatatype << ' ' << name
    val is = Knowledge.simd_instructionSet
    is match {
      case "QPX" =>
        out << ";\n"
        out << "{\n"
        out << " double _a[4] __attribute__((aligned(32))) = { 0, 1, 2, 3 };\n"
        out << ' ' << name << " = vec_lda(0, _a);\n"
        out << "}"

      case "SSE3" | "AVX" | "AVX2" =>
        val bit = if (is == "SSE3") "" else "256"
        val prec = if (Knowledge.useDblPrecision) 'd' else 's'
        out << " = _mm" << bit << "_set_p" << prec << '('
        for (i <- Knowledge.simd_vectorSize - 1 to 0 by -1)
          out << i << ','
        out.removeLast()
        out << ");"

      case "NEON" =>
        out << ";\n"
        out << "{\n"
        out << " float _a[4] = { 0, 1, 2, 3 };\n"
        out << ' ' << name << " = vld1q_f32(_a);\n"
        out << "}"
    }
  }
}
