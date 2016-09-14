package exastencils.simd

import exastencils.base.ir._
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream

/// IR_SIMD_HorizontalAdd

case class IR_SIMD_HorizontalAdd(var dest : IR_Expression, var src : IR_Expression) extends IR_Statement {
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
        IR_SIMD_HorizontalPrinterHelper.prettyprint(out, dest, src, "add", "+=")
    }
  }
}

/// IR_SIMD_HorizontalMul

case class IR_SIMD_HorizontalMul(var dest : IR_Expression, var src : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = IR_SIMD_HorizontalPrinterHelper.prettyprint(out, dest, src, "mul", "*=")
}

/// IR_SIMD_HorizontalMin

case class IR_SIMD_HorizontalMin(var dest : IR_Expression, var src : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "QPX")
      out << "NOT VALID ; vec_min not available on BG/Q" // FIXME: cmp and sel!
    else
      IR_SIMD_HorizontalPrinterHelper.prettyprint(out, dest, src, "min", "=", "std::min")
  }
}

/// IR_SIMD_HorizontalMax

case class IR_SIMD_HorizontalMax(var dest : IR_Expression, var src : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "QPX")
      out << "NOT VALID ; vec_max not available on BG/Q" // FIXME: cmp and sel!
    else
      IR_SIMD_HorizontalPrinterHelper.prettyprint(out, dest, src, "max", "=", "std::max")
  }
}
