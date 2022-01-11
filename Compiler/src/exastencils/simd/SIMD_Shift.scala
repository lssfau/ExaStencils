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
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.config._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// SIMD_ConcShift

// TODO: why is offset val?
case class SIMD_ConcShift(var left : IR_VariableAccess, var right : IR_VariableAccess, val offset : Int) extends SIMD_Expression {
  private var shiftIV : IR_IV_VecShiftIndex = null

  Platform.simd_instructionSet match {
    case "AVX512" | "IMCI" => shiftIV = IR_IV_VecShiftIndex(offset)
    case _                 =>
  }

  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    Platform.simd_instructionSet match { // TODO: use palignr
      case "SSE3" =>
        if (Knowledge.useDblPrecision) offset match {
          case 1 => out << "_mm_shuffle_pd(" << left << ", " << right << ", 1)"
        }
        else offset match {
          case 1 => out << "_mm_shuffle_ps(" << left << ", _mm_shuffle_ps(" << right << ", " << left << ", 0x30), 0x29)"
          case 2 => out << "_mm_shuffle_ps(" << left << ", " << right << ", 0x4E)"
          case 3 => out << "_mm_shuffle_ps(_mm_shuffle_ps(" << left << ", " << right << ", 0x3), " << right << ", 0x98)"
        }

      case "AVX" | "AVX2" =>
        if (Knowledge.useDblPrecision) offset match {
          case 1 => out << "_mm256_shuffle_pd(" << left << ", _mm256_permute2f128_pd(" << left << ", " << right << ", 0x21), 0x5)"
          case 2 => out << "_mm256_permute2f128_pd(" << left << ", " << right << ", 0x21)"
          case 3 => out << "_mm256_shuffle_pd(_mm256_permute2f128_pd(" << left << ", " << right << ", 0x21), " << right << ", 0x5)"
        }
        else offset match {
          case 1 => out << "_mm256_permute_ps(_mm256_blend_ps(" << left << ",_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), 0x11), 0x39)"
          case 2 => out << "_mm256_shuffle_ps(" << left << ", _mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), 0x4E)"
          case 3 => out << "_mm256_permute_ps(_mm256_blend_ps(" << left << ",_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), 0x77), 0x93)"
          case 4 => out << "_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21)"
          case 5 => out << "_mm256_permute_ps(_mm256_blend_ps(_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), " << right << ", 0x11), 0x39)"
          case 6 => out << "_mm256_shuffle_ps(_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), " << right << ", 0x4E)"
          case 7 => out << "_mm256_permute_ps(_mm256_blend_ps(_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), " << right << ", 0x77), 0x93)"
        }

      case "AVX512" =>
        if (offset <= 0 || offset >= Platform.simd_vectorSize)
          throw new InternalError("offset for SIMD_ConcShift out of bounds: " + offset)
        val prec = if (Knowledge.useDblPrecision) 'd' else 's'
        out << "_mm512_permutex2var_p" << prec << '(' << left << ", " << shiftIV << ", " << right << ')'

      case "IMCI" =>
        if (Knowledge.useDblPrecision) offset match {
          case 1 => out << "_mm512_castsi512_pd(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castpd_si512(_mm512_mask_blend_pd(0x01, " << left << ", " << right << "))))"
          case 2 => out << "_mm512_castsi512_pd(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x03, " << left << ", " << right << ")), _MM_PERM_ADCB))"
          case 3 => out << "_mm512_castsi512_pd(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castpd_si512(_mm512_mask_blend_pd(0x07, " << left << ", " << right << "))))"
          case 4 => out << "_mm512_castsi512_pd(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x0F, " << left << ", " << right << ")), _MM_PERM_BADC))"
          case 5 => out << "_mm512_castsi512_pd(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castpd_si512(_mm512_mask_blend_pd(0x1F, " << left << ", " << right << "))))"
          case 6 => out << "_mm512_castsi512_pd(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x3F, " << left << ", " << right << ")), _MM_PERM_CBAD))"
          case 7 => out << "_mm512_castsi512_pd(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castpd_si512(_mm512_mask_blend_pd(0x7F, " << left << ", " << right << "))))"
        }
        else offset match {
          case 1  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x0001, " << left << ", " << right << "))))"
          case 2  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x0003, " << left << ", " << right << "))))"
          case 3  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x0007, " << left << ", " << right << "))))"
          case 4  => out << "_mm512_castsi512_ps(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x000F, " << left << ", " << right << ")), _MM_PERM_ADCB))"
          case 5  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x001F, " << left << ", " << right << "))))"
          case 6  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x003F, " << left << ", " << right << "))))"
          case 7  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x007F, " << left << ", " << right << "))))"
          case 8  => out << "_mm512_castsi512_ps(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x00FF, " << left << ", " << right << ")), _MM_PERM_BADC))"
          case 9  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x01FF, " << left << ", " << right << "))))"
          case 10 => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x03FF, " << left << ", " << right << "))))"
          case 11 => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x07FF, " << left << ", " << right << "))))"
          case 12 => out << "_mm512_castsi512_ps(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x0FFF, " << left << ", " << right << ")), _MM_PERM_CBAD))"
          case 13 => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x1FFF, " << left << ", " << right << "))))"
          case 14 => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x3FFF, " << left << ", " << right << "))))"
          case 15 => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x7FFF, " << left << ", " << right << "))))"
        }

      case "QPX"  => out << "vec_sldw(" << left << ", " << right << ", " << offset << ")"
      case "NEON" => out << "vextq_" << (if (Knowledge.useDblPrecision) "f64" else "f32") << "(" << left << ", " << right << ", " << offset << ")"
    }
  }
}

/// IR_VecShiftIndexStaticInit

object IR_VecShiftIndexStaticInit {
  val header = Platform.simd_header
  if (header != null)
    IR_GlobalCollection.get.externalDependencies += header
}

/// IR_IV_VecShiftIndex

case class IR_IV_VecShiftIndex(var offset : Int) extends IR_UnduplicatedVariable {
  IR_VecShiftIndexStaticInit // just to ensure VecShiftIndexStaticInit is initialized (once, since its an object)

  if (offset <= 0 || offset >= Platform.simd_vectorSize)
    Logger.error("VecShiftIndex out of bounds: " + offset)

  override def resolveName() = "vShift" + offset
  override def resolveDatatype() = IR_SpecialDatatype("__m512i")

  override def getCtor() : Option[IR_Statement] = {
    val init = IR_StringLiteral(null : String)
    Platform.simd_instructionSet match {
      case "AVX512" =>
        if (Knowledge.useDblPrecision)
          init.value = "_mm512_set_epi64(" + (7 + offset to 0 + offset by -1).mkString(", ") + ')'
        else
          init.value = "_mm512_set_epi32(" + (15 + offset to 0 + offset by -1).mkString(", ") + ')'

      case "IMCI" =>
        val stride : Int = if (Knowledge.useDblPrecision) 2 else 1
        init.value = "_mm512_cvtfxpnt_round_adjustps_epi32((__m512) { " + (0 to 15).map(i => (i + offset * stride) % 16).mkString(", ") + " }, _MM_FROUND_TO_NEAREST_INT, _MM_EXPADJ_NONE)"

      case si => Logger.error("VecShiftIndex cannot be used for instruction set " + si)
    }

    Some(IR_Assignment(IR_VariableAccess(resolveName(), resolveDatatype()), init))
  }
}
