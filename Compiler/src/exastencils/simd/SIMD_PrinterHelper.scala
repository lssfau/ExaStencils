//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.simd

import exastencils.base.ir._
import exastencils.config._
import exastencils.prettyprinting.PpStream

/// SIMD_HorizontalPrinterHelper

private object SIMD_HorizontalPrinterHelper {
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
        if (Knowledge.useDblPrecision) {
          out << " float64x2_t _v = " << src << ";\n"
          out << " double _r = vget_lane_f64(_v,0);\n"
          out << " _r " << assOp
          if (redFunc != null)
            out << ' ' << redFunc << "(_r, vget_lane_f64(_v,1));\n"
          else
            out << " vget_lane_f64(_v,1);\n"
        } else {
          out << " float32x4_t _v = " << src << ";\n"
          out << " float32x2_t _w = v" << redName << "_f32(vget_high_f32(_v), vget_low_f32(_v));\n"
          out << " float _r = vget_lane_f32(_w,0);\n"
          out << " _r " << assOp
          if (redFunc != null)
            out << ' ' << redFunc << "(_r, vget_lane_f32(_w,1));\n"
          else
            out << " vget_lane_f32(_w,1);\n"
        }
    }
    out << dest << ' ' << assOp
    if (redFunc != null)
      out << ' ' << redFunc << '(' << dest << ",_r);\n"
    else
      out << " _r;\n"
    out << '}'
  }
}

/// SIMD_FusedPrinterHelper

private object SIMD_FusedPrinterHelper {
  def prettyprint(out : PpStream, factor1 : IR_Expression, factor2 : IR_Expression, summand : IR_Expression, addSub : String) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_" << addSub << "_p" << prec << "(_mm_mul_p" << prec << '(' << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX"             => out << "_mm256_" << addSub << "_p" << prec << "(_mm256_mul_p" << prec << '(' << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX2"            => out << "_mm256_fm" << addSub << "_p" << prec << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "AVX512" | "IMCI" => out << "_mm512_fm" << addSub << "_p" << prec << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "QPX"             => out << "vec_m" << addSub << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "NEON"            =>
        val prec_arm = (if (Knowledge.useDblPrecision) "f64" else "f32")
        if (addSub == "add")
          out << "vmlaq_" << prec_arm << "(" << summand << ", " << factor1 << ", " << factor2 << ')' // use unfused for compatibility with gcc 4.7 and older
        else // vmlsq_f32(a,b,c) is a-b*c and not a*b-c; thanks ARM  -.-
          out << "vnegq_" << prec_arm << "(vmlsq_" << prec_arm << "(" << summand << ", " << factor1 << ", " << factor2 << "))"
    }
  }
}
