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
import exastencils.optimization.ir.VectorizationException
import exastencils.prettyprinting.PpStream

trait SIMD_Datatype extends IR_Datatype {
  def datatype : IR_ScalarDatatype

  // TODO: currently treated similar to a vector - correct?

  override def dimensionality : Int = 1
  override def getSizeArray : Array[Int] = Array(Platform.simd_vectorSize)
  override def resolveBaseDatatype : IR_Datatype = datatype
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = Platform.simd_vectorSize
  override def typicalByteSize = Platform.simd_vectorSize * datatype.typicalByteSize
}

case object SIMD_RealDatatype extends SIMD_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def datatype : IR_ScalarDatatype = IR_RealDatatype

  override def prettyprint(out : PpStream) : Unit = {
    val suffix = if (Knowledge.useDblPrecision) "d" else ""
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "__m128" << suffix
      case "AVX" | "AVX2"    => out << "__m256" << suffix
      case "AVX512" | "IMCI" => out << "__m512" << suffix
      case "QPX"             => out << "vector4double" // no suffix
      case "NEON"            => out << (if (Knowledge.useDblPrecision) "float64x2_t" else "float32x4_t")
    }
  }

  override def prettyprint_mpi = "INVALID DATATYPE: " + this.prettyprint()
}

// AVX512 comparison returns _mmask datatypes, SIMD_RealDatatypes for others

case object SIMD_MaskDatatype extends SIMD_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def datatype : IR_ScalarDatatype = IR_RealDatatype

  override def prettyprint(out : PpStream) : Unit = {
    Platform.simd_instructionSet match {
      case "SSE3" | "AVX" | "AVX2" => out << SIMD_RealDatatype.prettyprint()
      case "AVX512"                => out << "__mmask" << Platform.simd_vectorSize
      case "NEON"                  => out << (if (Knowledge.useDblPrecision) "uint64x2_t" else "uint32x4_t")
      case "IMCI"                  => new VectorizationException("SIMD_MaskDatatype: Currently unsupported for " + Platform.simd_instructionSet)
      case "QPX"                   => new VectorizationException("SIMD_MaskDatatype: Currently unsupported for " + Platform.simd_instructionSet)
    }
  }

  override def prettyprint_mpi = "INVALID DATATYPE: " + this.prettyprint()
}
