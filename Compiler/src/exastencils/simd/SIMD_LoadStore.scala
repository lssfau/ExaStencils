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

/// SIMD_Load

// TODO: why is aligned val?
case class SIMD_Load(var mem : IR_Expression, val aligned : Boolean) extends SIMD_Expression {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    val alig = if (aligned) "" else "u"
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_load" << alig << "_p" << prec << '('
      case "AVX" | "AVX2" => out << "_mm256_load" << alig << "_p" << prec << '('
      case "AVX512"       => out << "_mm512_load" << alig << "_p" << prec << '('
      case "IMCI"         => if (aligned) out << "_mm512_load_p" << prec << '(' else throw new InternalError("IMCI does not support unaligned loads")
      case "QPX"          => if (aligned) out << "vec_lda(0," else throw new InternalError("QPX does not support unaligned loads")
      case "NEON"         => out << "vld1q_f32(" // TODO: only unaligned?
    }
    out << mem << ')'
  }
}

/// SIMD_Store

case class SIMD_Store(var mem : IR_Expression, var value : IR_Expression, var aligned : Boolean) extends SIMD_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    val alig = if (aligned) "" else "u"
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_store" << alig << "_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_store" << alig << "_p" << prec
      case "AVX512"       => out << "_mm512_store" << alig << "_p" << prec
      case "IMCI"         => out << (if (aligned) "_mm512_store_p" + prec else "\n --- NOT VALID ; unaligned store for QPX: \n")
      case "QPX"          => out << (if (aligned) "vec_sta" else "\n --- NOT VALID ; unaligned store for QPX: \n")
      case "NEON"         => out << "vst1q_f32"
    }
    Platform.simd_instructionSet match {
      case "QPX" => out << '(' << value << ", 0, " << mem << ");"
      case _     => out << '(' << mem << ", " << value << ");"
    }
  }
}

/// SIMD_MaskStore

case class SIMD_MaskStore(var mem : IR_Expression, var value : IR_Expression, var mask : IR_Expression, var aligned : Boolean) extends SIMD_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    val alig = if (aligned) "" else "u"
    Platform.simd_instructionSet match {
      case "SSE3"         => Logger.error("Unsupported instruction")
      case "AVX" | "AVX2" => out << "_mm256_mask_store" << alig << "_p" << prec
      case "AVX512"       => out << "_mm512_mask_store" << alig << "_p" << prec
      case "IMCI"         => Logger.error("Unsupported instruction")
      case "QPX"          => Logger.error("Unsupported instruction")
      case "NEON"         => out << "vst1q_f32"
    }
    out << '(' << mem << ", " << mask << "," << value << ");"
  }
}
