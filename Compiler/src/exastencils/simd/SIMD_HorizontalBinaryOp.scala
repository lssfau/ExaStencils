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

/// SIMD_HorizontalAdd

case class SIMD_HorizontalAdd(var dest : IR_Expression, var src : IR_Expression) extends SIMD_Statement {
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
        SIMD_HorizontalPrinterHelper.prettyprint(out, dest, src, "add", "+=")
    }
  }
}

/// SIMD_HorizontalMul

case class SIMD_HorizontalMul(var dest : IR_Expression, var src : IR_Expression) extends SIMD_Statement {
  override def prettyprint(out : PpStream) : Unit = SIMD_HorizontalPrinterHelper.prettyprint(out, dest, src, "mul", "*=")
}

/// SIMD_HorizontalMin

case class SIMD_HorizontalMin(var dest : IR_Expression, var src : IR_Expression) extends SIMD_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "QPX")
      out << "\n --- NOT VALID ; vec_min not available on BG/Q\n" // FIXME: cmp and sel!
    else
      SIMD_HorizontalPrinterHelper.prettyprint(out, dest, src, "min", "=", "std::min")
  }
}

/// SIMD_HorizontalMax

case class SIMD_HorizontalMax(var dest : IR_Expression, var src : IR_Expression) extends SIMD_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "QPX")
      out << "\n --- NOT VALID ; vec_max not available on BG/Q\n" // FIXME: cmp and sel!
    else
      SIMD_HorizontalPrinterHelper.prettyprint(out, dest, src, "max", "=", "std::max")
  }
}
