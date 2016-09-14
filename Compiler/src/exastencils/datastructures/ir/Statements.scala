package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation._
import exastencils.knowledge._
import exastencils.prettyprinting._
import exastencils.util._

case class VariableDeclarationStatement(var datatype : IR_Datatype, var name : String, var expression : Option[IR_Expression] = None) extends IR_Statement {
  var alignment : Int = 1
  def this(dT : IR_Datatype, n : String, e : IR_Expression) = this(dT, n, Option(e))
  def this(va : IR_VariableAccess) = this(va.datatype.get, va.name, None)
  def this(va : IR_VariableAccess, e : IR_Expression) = this(va.datatype.get, va.name, Option(e))

  override def prettyprint(out : PpStream) : Unit = {
    datatype match {
      case x : IR_VectorDatatype => {
        out << x << ' ' << name
        if (expression.isDefined) {
          out << "("
          expression.get.asInstanceOf[VectorExpression].prettyprintInner(out)
          out << ")"
        }
      }
      case x : IR_MatrixDatatype => {
        out << x << ' ' << name
        if (expression.isDefined) {
          out << "("
          expression.get.asInstanceOf[MatrixExpression].prettyprintInner(out)
          out << ")"
        }
      }
      case _                     => {
        if (alignment > 1 && "MSVC" == Platform.targetCompiler)
          out << "__declspec(align(" << alignment * 8 << ")) "
        out << datatype.resolveDeclType << ' ' << name << datatype.resolveDeclPostscript
        if (alignment > 1 && "MSVC" != Platform.targetCompiler)
          out << " __attribute__((aligned(" << alignment * 8 << ")))"
        if (expression.isDefined)
          out << " = " << expression.get
      }
    }

    out << ';'
  }

  def prettyprint_onlyDeclaration() : String = VariableDeclarationStatement(datatype, name, None).prettyprint()
}

case class ObjectInstantiation(var datatype : IR_Datatype, var name : String, var ctorArgs : ListBuffer[IR_Expression]) extends IR_Statement {
  def this(datatype : IR_Datatype, name : String, ctorArgs : IR_Expression*) = this(datatype, name, ctorArgs.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << datatype.resolveDeclType << ' ' << name << datatype.resolveDeclPostscript
    if (ctorArgs.length > 0)
      out << '(' <<< (ctorArgs, ", ") << ')'
    out << ';'
  }
}

case class FreeStatement(var pointer : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "delete[] " << pointer << ";"
  }
}

case class DefineStatement(var name : IR_Expression, var value : Option[IR_Expression] = None) extends IR_Statement {
  def this(n : IR_Expression, v : IR_Expression) = this(n, Option(v))

  override def prettyprint(out : PpStream) : Unit = {
    out << "#define " << name
    if (value.isDefined)
      out << ' ' << value.get
  }
}

case class CommentStatement(var comment : String) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "/* " << comment << " */"
}

case class AssignmentStatement(var dest : IR_Expression, var src : IR_Expression, var op : String = "=") extends IR_Statement {
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

case class WhileLoopStatement(var comparison : IR_Expression, var body : ListBuffer[IR_Statement]) extends IR_Statement {
  def this(comparison : IR_Expression, body : IR_Statement*) = this(comparison, body.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << "while (" << comparison << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class ForLoopStatement(var begin : IR_Statement, var end : IR_Expression, var inc : IR_Statement, var body : ListBuffer[IR_Statement], var reduction : Option[Reduction] = None) extends IR_Statement {
  def this(begin : IR_Statement, end : IR_Expression, inc : IR_Statement, reduction : Reduction, body : IR_Statement*) = this(begin, end, inc, body.to[ListBuffer], Option(reduction))
  def this(begin : IR_Statement, end : IR_Expression, inc : IR_Statement, body : IR_Statement*) = this(begin, end, inc, body.to[ListBuffer])

  def maxIterationCount() = {
    if (hasAnnotation("numLoopIterations"))
      getAnnotation("numLoopIterations").get.asInstanceOf[Int]
    else
      0 // TODO: warning?
  }

  override def prettyprint(out : PpStream) : Unit = {
    // BEGIN AMAZING HACK as workaround for IBM XL compiler
    var realEnd = end.prettyprint(out.env)
    if (realEnd.size > 2 && realEnd(0) == '(')
      realEnd = realEnd.substring(1, realEnd.size - 1)
    var realInc = inc.prettyprint(out.env)
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

case class ConditionStatement(var condition : IR_Expression, var trueBody : ListBuffer[IR_Statement], var falseBody : ListBuffer[IR_Statement]) extends IR_Statement {
  def this(condition : IR_Expression, trueBody : ListBuffer[IR_Statement]) = this(condition, trueBody, ListBuffer[IR_Statement]())
  def this(condition : IR_Expression, trueBranch : IR_Statement) = this(condition, ListBuffer(trueBranch))

  def this(condition : IR_Expression, trueBranch : IR_Statement, falseBranch : IR_Statement) = this(condition, ListBuffer(trueBranch), ListBuffer(falseBranch))
  def this(condition : IR_Expression, trueBody : ListBuffer[IR_Statement], falseBranch : IR_Statement) = this(condition, trueBody, ListBuffer(falseBranch))
  def this(condition : IR_Expression, trueBranch : IR_Statement, falseBody : ListBuffer[IR_Statement]) = this(condition, ListBuffer(trueBranch), falseBody)

  override def prettyprint(out : PpStream) : Unit = {
    out << "if (" << condition << ") {\n"
    out <<< (trueBody, "\n") << '\n'
    if (!falseBody.isEmpty) {
      out << "} else {\n"
      out <<< (falseBody, "\n") << '\n'
    }
    out << '}'
  }
}

case class CaseStatement(var toMatch : IR_Expression, var body : ListBuffer[IR_Statement]) extends IR_Statement {
  def this(toMatch : IR_Expression, body : IR_Statement*) = this(toMatch, body.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << "case " << toMatch << ": {\n"
    out <<< (body, "\n") << '\n'
    out << "} break;"
  }
}

case class SwitchStatement(var what : IR_Expression, var body : ListBuffer[CaseStatement]) extends IR_Statement {
  def this(what : IR_Expression, body : CaseStatement*) = this(what, body.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << "switch (" << what << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

case class ReturnStatement(var expr : Option[IR_Expression] = None) extends IR_Statement {
  def this(expr : IR_Expression) = this(Option(expr))

  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
    out << ';'
  }
}

case class BreakStatement() extends IR_Statement {
  override def prettyprint(out : PpStream) = {
    out << "break;"
  }
}

case class AssertStatement(var check : IR_Expression, var msg : ListBuffer[IR_Expression], var abort : IR_Statement) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = AssertStatement\n"

  override def expand : Output[ConditionStatement] = {
    new ConditionStatement(IR_NegationExpression(check),
      ListBuffer[IR_Statement](new PrintStatement(msg), abort))
  }
}

abstract class AbstractFunctionStatement(var isHeaderOnly : Boolean = false) extends IR_Statement {
  def name : String
  def prettyprint_decl() : String
}

case class FunctionStatement(
    var returntype : IR_Datatype,
    var name : String,
    var parameters : ListBuffer[FunctionArgument],
    var body : ListBuffer[IR_Statement],
    var allowInlining : Boolean = true,
    var allowFortranInterface : Boolean = true,
    var functionQualifiers : String = "" // e.g. "__global__" etc
) extends AbstractFunctionStatement {
  def this(returntype : IR_Datatype, name : String, parameters : ListBuffer[FunctionArgument], body : IR_Statement) = this(returntype, name, parameters, ListBuffer[IR_Statement](body))
  def this(returntype : IR_Datatype, name : String, parameters : FunctionArgument, body : ListBuffer[IR_Statement]) = this(returntype, name, ListBuffer[FunctionArgument](parameters), body)

  override def prettyprint(out : PpStream) : Unit = { // FIXME: add specialized node for parameter specification with own PP
    if (!functionQualifiers.isEmpty) out << functionQualifiers << ' '
    out << returntype << ' ' << name << ' ' << '('
    if (!parameters.isEmpty) {
      for (param <- parameters)
        out << param.prettyprintDeclaration << ", "
      out.removeLast(2)
    }
    out << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def prettyprint_decl() : String = {
    var decl = ""
    if (!functionQualifiers.isEmpty) decl += functionQualifiers + ' '
    decl += s"${ returntype.prettyprint } $name (" + parameters.map(param => s"${ param.prettyprintDeclaration }").mkString(", ") + ");\n"
    decl
  }
}

case class FunctionArgument(var name : String, var datatype : IR_Datatype) extends IR_Expression {
  // FIXME: really Expression?
  override def prettyprint(out : PpStream) = {
    out << name
  }
  def prettyprintDeclaration = s"${ datatype.prettyprint } ${ name }"
}

//////////////////////////// SIMD Statements \\\\\\\\\\\\\\\\\\\\\\\\\\\\

case class SIMD_StoreStatement(var mem : IR_Expression, var value : IR_Expression, var aligned : Boolean) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    val alig = if (aligned) "" else "u"
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_store" << alig << "_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_store" << alig << "_p" << prec
      case "AVX512"       => out << "_mm512_store" << alig << "_p" << prec
      case "IMCI"         => out << (if (aligned) "_mm512_store_p" + prec else "NOT VALID ; unaligned store for QPX: ")
      case "QPX"          => out << (if (aligned) "vec_sta" else "NOT VALID ; unaligned store for QPX: ")
      case "NEON"         => out << "vst1q_f32"
    }
    Platform.simd_instructionSet match {
      case "QPX" => out << '(' << value << ", 0, " << mem << ");"
      case _     => out << '(' << mem << ", " << value << ");"
    }
  }
}

case class SIMD_HorizontalAddStatement(var dest : IR_Expression, var src : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    Platform.simd_instructionSet match {
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

      case _ =>
        HorizontalPrinterHelper.prettyprint(out, dest, src, "add", "+=")
    }
  }
}

case class SIMD_HorizontalMulStatement(var dest : IR_Expression, var src : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    HorizontalPrinterHelper.prettyprint(out, dest, src, "mul", "*=")
  }
}

case class SIMD_HorizontalMinStatement(var dest : IR_Expression, var src : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "QPX")
      out << "NOT VALID ; vec_min not available on BG/Q" // FIXME: cmp and sel!
    else
      HorizontalPrinterHelper.prettyprint(out, dest, src, "min", "=", "std::min")
  }
}

case class SIMD_HorizontalMaxStatement(var dest : IR_Expression, var src : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "QPX")
      out << "NOT VALID ; vec_max not available on BG/Q" // FIXME: cmp and sel!
    else
      HorizontalPrinterHelper.prettyprint(out, dest, src, "max", "=", "std::max")
  }
}

private object HorizontalPrinterHelper {
  def prettyprint(out : PpStream, dest : IR_Expression, src : IR_Expression, redName : String, assOp : String, redFunc : String = null) : Unit = {
    out << "{\n"
    Platform.simd_instructionSet match {
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

      case "AVX512" | "IMCI" =>
        if (Knowledge.useDblPrecision)
          out << " double _r = _mm512_reduce_" << redName << "_pd(" << src << ");\n"
        else
          out << " float  _r = _mm512_reduce_" << redName << "_ps(" << src << ");\n"

      case "QPX" =>
        out << " vector4double _v = " << src << ";\n"
        out << " _v = vec_" << redName << "(_v, vec_sldw(_v, _v, 2));\n"
        out << ' ' << IR_RealDatatype << " _r = (" << IR_RealDatatype << ") vec_extract(vec_" << redName << "(_v, vec_sldw(_v, _v, 1)), 0);\n"

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
case class SIMD_IncrementVectorDeclaration(var name : String, var incr : Long) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << SIMD_RealDatatype << ' ' << name
    val is = Platform.simd_instructionSet
    is match {
      case "QPX" =>
        out << ";\n"
        out << "{\n"
        out << " double _a[4] __attribute__((aligned(32))) = { 0, " << incr << ", " << 2 * incr << ", " << 3 * incr << " };\n"
        out << ' ' << name << " = vec_lda(0, _a);\n"
        out << "}"

      case "SSE3" | "AVX" | "AVX2" | "AVX512" =>
        val bit = if (is == "SSE3") "" else if (is == "AVX512") "512" else "256"
        val prec = if (Knowledge.useDblPrecision) 'd' else 's'
        out << " = _mm" << bit << "_set_p" << prec << '('
        for (i <- Platform.simd_vectorSize - 1 to 1 by -1)
          out << i * incr << ", "
        out << "0);"

      case "IMCI" =>
        out << " (" << SIMD_RealDatatype << ") { 0"
        for (i <- 1 until Platform.simd_vectorSize)
          out << ", " << i * incr
        out << " };"

      case "NEON" =>
        out << ";\n"
        out << "{\n"
        out << " float _a[4] = { 0, " << incr << ", " << 2 * incr << ", " << 3 * incr << " };\n"
        out << ' ' << name << " = vld1q_f32(_a);\n"
        out << "}"
    }
  }
}
