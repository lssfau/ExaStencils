package exastencils.simd

import exastencils.base.ir._
import exastencils.datastructures.ir.iv
import exastencils.config._
import exastencils.prettyprinting.PpStream

/// IR_SIMD_ConcShift

// TODO: why is offset val?
case class IR_SIMD_ConcShift(var left : IR_VariableAccess, var right : IR_VariableAccess, val offset : Int) extends IR_SIMD_Expression {
  private var shiftIV : iv.VecShiftIndex = null

  Platform.simd_instructionSet match {
    case "AVX512" | "IMCI" => shiftIV = new iv.VecShiftIndex(offset)
    case _                 =>
  }

  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    Platform.simd_instructionSet match {
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
      case "NEON" => out << "vextq_f32(" << left << ", " << right << ", " << offset << ")" // TODO: only single precision?
    }
  }
}
