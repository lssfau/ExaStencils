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

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_RealDatatype
import exastencils.config._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// SIMD_ExtractScalar

case class SIMD_ExtractScalar(var expr : IR_Expression, var index : Int) extends SIMD_Expression {
  override def datatype = IR_RealDatatype
  override def prettyprint(out : PpStream) : Unit = {
    out << expr
    if (Platform.targetCompiler == "MSVC")
      (Platform.simd_instructionSet, Knowledge.useDblPrecision) match {
        case ("SSE3", false)         => out << ".m128d_f32"
        case ("SSE3", true)          => out << ".m128d_f64"
        case ("AVX" | "AVX2", false) => out << ".m256d_f32"
        case ("AVX" | "AVX2", true)  => out << ".m256d_f64"
        case _                       => Logger.error("SIMD_ExtractScalarExpression for MSVC compiler and instruction set " + Platform.simd_instructionSet + " not implemented yet")
      }
    out << '[' << index << ']' // TODO: check if this works with all instruction sets and compiler
  }
}

/// SIMD_Scalar2Vector

case class SIMD_Scalar2Vector(var scalar : IR_Expression) extends SIMD_Expression {
  override def datatype = SIMD_RealDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_set1_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_set1_p" << prec
      case "AVX512"       => out << "_mm512_set1_p" << prec
      case "IMCI"         => out << "_mm512_set_1to" << Platform.simd_vectorSize << "_p" << prec
      case "QPX"          => out << "vec_splats"
      case "NEON"         => out << "vdupq_n_" << (if (Knowledge.useDblPrecision) "f64" else "f32")
    }
    out << '(' << scalar << ')'
  }
}
