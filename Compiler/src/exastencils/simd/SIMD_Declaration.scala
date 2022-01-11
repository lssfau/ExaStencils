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

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IntegerConstant
import exastencils.config._
import exastencils.prettyprinting.PpStream

/// SIMD_Scalars2Vector

case class SIMD_Scalars2Vector(var name : String, var scalars : ListBuffer[IR_Expression]) extends SIMD_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << SIMD_RealDatatype << ' ' << name
    val is = Platform.simd_instructionSet
    is match {
      case "QPX" =>
        out << ";\n"
        out << "{\n"
        out << " double _a[4] __attribute__((aligned(32))) = { " <<< (scalars, ", ") << " };\n"
        out << ' ' << name << " = vec_lda(0, _a);\n"
        out << "}"

      case "SSE3" | "AVX" | "AVX2" | "AVX512" =>
        val bit = if (is == "SSE3") "" else if (is == "AVX512") "512" else "256"
        val prec = if (Knowledge.useDblPrecision) 'd' else 's'
        out << " = _mm" << bit << "_set_p" << prec << '(' <<< (scalars.view.reverse, ", ") << ");"

      case "IMCI" =>
        out << " = (" << SIMD_RealDatatype << ") { " <<< (scalars, ", ") << " };"

      case "NEON" =>
        out << ";\n"
        out << "{\n"
        if (Knowledge.useDblPrecision) {
          out << " double _a[2] = { " <<< (scalars, ", ") << " };\n"
          out << ' ' << name << " = vld1q_f64(_a);\n"
        } else {
          out << " float _a[4] = { " <<< (scalars, ", ") << " };\n"
          out << ' ' << name << " = vld1q_f32(_a);\n"
        }
        out << "}"
    }
  }
}

/// SIMD_IncrementVectorDeclaration

/** Special declaration for a SIMD vector, which is initialized with the values 0, 1, ..., Knowledge.simd_vectorSize-1. */
object SIMD_IncrementVectorDeclaration {
  def apply(name : String, incr : Long) : SIMD_Scalars2Vector = {
    val scalars = ListBuffer[IR_Expression]()
    for (i <- 0 until Platform.simd_vectorSize)
      scalars += IR_IntegerConstant(incr * i)
    SIMD_Scalars2Vector(name, scalars)
  }
}
