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
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// SIMD_Negate

case class SIMD_Negate(var vect : IR_Expression) extends SIMD_Expression {
  override def datatype = vect.datatype
  override def prettyprint(out : PpStream) : Unit = {
    val (prec, ts) = if (Knowledge.useDblPrecision) ('d', "d") else ('s', "")
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_xor_p" << prec << '(' << vect << ", _mm_set1_p" << prec << "(-0.0))"
      case "AVX" | "AVX2" => out << "_mm256_xor_p" << prec << '(' << vect << ", _mm256_set1_p" << prec << "(-0.0))"
      case "AVX512"       => out << "_mm512_xor_p" << prec << '(' << vect << ", _mm512_set1_p" << prec << "(-0.0))"
      case "IMCI"         => out << "_mm512_sub_p" << prec << "((__m512" << ts << ") 0, " << vect << ")" // TODO: is there a more efficient version?
      case "QPX"          => out << "vec_neg(" << vect << ')'
      case "NEON"         => out << "vnegq_" << (if (Knowledge.useDblPrecision) "f64" else "f32") << "(" << vect << ')'
    }
  }
}

case class SIMD_MoveMask(var mask : IR_Expression) extends SIMD_Expression {
  override def datatype : IR_Datatype = IR_IntegerDatatype

  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_movemask_p" << prec <<"(" << mask << ")"
      case "AVX" | "AVX2" => out << "_mm256_movemask_p" << prec << "(" << mask << ")"
      case "AVX512"       => out << "_mm512_mask2int(" << mask << ")"
      // TODO
      case "IMCI"         => out << Logger.error("MoveMask not implemented for: " + Platform.simd_instructionSet)
      case "QPX"          => out << Logger.error("MoveMask not implemented for: " + Platform.simd_instructionSet)
      case "NEON"         => out << Logger.error("MoveMask not implemented for: " + Platform.simd_instructionSet)
    }
  }
}
