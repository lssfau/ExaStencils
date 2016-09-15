package exastencils.simd

import exastencils.base.ir._
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream

/// IR_SIMD_HorizontalPrinterHelper

private object IR_SIMD_HorizontalPrinterHelper {
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
