package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.strategies._

@deprecated("should be removed completely, since it complicates AST analysis for transformations/optimization; please, don't use it in new code", "14.04.2016")
case class ConcatenationExpression(var expressions : ListBuffer[IR_Expression]) extends IR_Expression {
  def this(exprs : IR_Expression*) = this(exprs.to[ListBuffer])
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out <<< expressions
}

//TODO specific expression for reading from fragment data file
case class ReadValueFrom(var innerDatatype : IR_Datatype, data : IR_Expression) extends IR_Expression {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "readValue<" << innerDatatype << '>' << "(" << data << ")"
}

case class LoopCarriedCSBufferAccess(var buffer : iv.LoopCarriedCSBuffer, var index : IR_ExpressionIndex) extends IR_Expression {
  override def datatype = buffer.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def linearize() : IR_ArrayAccess = {
    if (buffer.dimSizes.isEmpty)
      return new IR_ArrayAccess(buffer, IR_IntegerConstant(0), Knowledge.data_alignFieldPointers)

    return new IR_ArrayAccess(buffer, Mapping.resolveMultiIdx(index, buffer.dimSizes), Knowledge.data_alignFieldPointers)
  }
}

case class VirtualFieldAccess(var fieldName : String,
    var level : IR_Expression,
    var index : IR_ExpressionIndex,
    var arrayIndex : Option[Int] = None,
    var fragIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_Expression {
  // FIXME: datatype
  override def datatype = IR_RealDatatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
}

case class StencilAccess(var stencil : Stencil) extends IR_Expression {
  override def datatype = stencil.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
}

case class StencilFieldAccess(var stencilFieldSelection : StencilFieldSelection, var index : IR_ExpressionIndex) extends IR_Expression {
  override def datatype = stencilFieldSelection.stencilField.stencil.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def buildStencil : Stencil = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()
    for (e <- 0 until stencilFieldSelection.stencil.entries.size) {
      val stencilFieldIdx = Duplicate(index)
      stencilFieldIdx(stencilFieldSelection.stencilField.field.fieldLayout.numDimsData - 1) = e // TODO: assumes last index is vector dimension
      val fieldSel = stencilFieldSelection.toFieldSelection
      fieldSel.arrayIndex = Some(e)
      entries += new StencilEntry(stencilFieldSelection.stencil.entries(e).offset, new IR_FieldAccess(fieldSel, stencilFieldIdx))
    }
    new Stencil("GENERATED_PLACEHOLDER_STENCIL", stencilFieldSelection.stencil.level, entries)
  }
}

case class MemberAccess(var base : IR_Access, var member : String) extends IR_Access {
  override def datatype = base.datatype
  override def prettyprint(out : PpStream) : Unit = out << base << '.' << member
}

case class DerefAccess(var base : IR_Access) extends IR_Access {
  override def datatype = base.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(*" << base << ')'
}

case class StencilConvolution(var stencil : Stencil, var fieldAccess : IR_FieldAccess) extends IR_Expression with IR_Expandable {
  override def datatype = GetResultingDatatype(stencil.datatype, fieldAccess.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def resolveEntry(idx : Int) : IR_Expression = {
    stencil.entries(idx).coefficient * new IR_FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencil.entries(idx).offset)
  }

  override def expand() : Output[IR_Expression] = {
    val ret : IR_Expression = stencil.entries.indices.view.map(idx => Duplicate(resolveEntry(idx))).reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

// TODO: update convolutions with new dimensionality logic

case class StencilFieldConvolution(var stencilFieldAccess : StencilFieldAccess, var fieldAccess : IR_FieldAccess) extends IR_Expression with IR_Expandable {
  override def datatype = GetResultingDatatype(stencilFieldAccess.datatype, fieldAccess.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def resolveEntry(idx : Int) : IR_Expression = {
    val stencilFieldIdx = Duplicate(stencilFieldAccess.index)
    stencilFieldIdx(Knowledge.dimensionality) = idx

    IR_FieldAccess(stencilFieldAccess.stencilFieldSelection.toFieldSelection, stencilFieldIdx) *
      new IR_FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencilFieldAccess.stencilFieldSelection.stencil.entries(idx).offset)
  }

  override def expand() : Output[IR_Expression] = {
    val ret : IR_Expression = stencilFieldAccess.stencilFieldSelection.stencil.entries.indices.view.map(idx => Duplicate(resolveEntry(idx))).reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

case class StencilStencilConvolution(var stencilLeft : Stencil, var stencilRight : Stencil) extends IR_Expression with IR_Expandable {
  override def datatype = GetResultingDatatype(stencilLeft.datatype, stencilRight.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[StencilAccess] = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()

    for (re <- stencilRight.entries) {
      for (le <- stencilLeft.entries) {
        val rightOffset = Duplicate(re.offset)

        val leftOffset = Duplicate(le.offset)
        if (stencilRight.level > stencilLeft.level) {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : IR_Expression) / 2 + leftOffset(d)
        } else {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : IR_Expression) + leftOffset(d)
        }

        val combOff = leftOffset
        ResolveCoordinates.replacement = rightOffset
        ResolveCoordinates.doUntilDoneStandalone(combOff)

        var combCoeff : IR_Expression = (re.coefficient * le.coefficient)
        SimplifyStrategy.doUntilDoneStandalone(combOff)
        SimplifyStrategy.doUntilDoneStandalone(combCoeff)
        val addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
        if (addToEntry.isDefined) {
          combCoeff += addToEntry.get.coefficient
          SimplifyStrategy.doUntilDoneStandalone(combCoeff)
          addToEntry.get.coefficient = combCoeff
        } else entries += new StencilEntry(combOff, combCoeff)
      }
    }

    StencilAccess(Stencil(stencilLeft.identifier + "_" + stencilRight.identifier, stencilLeft.level, entries))
  }
}

case class StencilFieldStencilConvolution(var stencilLeft : StencilFieldAccess, var stencilRight : Stencil) extends IR_Expression with IR_Expandable {
  override def datatype = GetResultingDatatype(stencilLeft.datatype, stencilRight.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[StencilAccess] = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()

    for (re <- stencilRight.entries) {
      for (e <- 0 until stencilLeft.stencilFieldSelection.stencil.entries.size) {
        val stencilFieldIdx = Duplicate(stencilLeft.index)
        stencilFieldIdx(Knowledge.dimensionality) = e
        for (dim <- 0 until Knowledge.dimensionality)
          stencilFieldIdx(dim) += re.offset(dim)
        val fieldSel = stencilLeft.stencilFieldSelection.toFieldSelection
        fieldSel.arrayIndex = Some(e)

        val rightOffset = Duplicate(re.offset)

        val leftOffset = Duplicate(stencilLeft.stencilFieldSelection.stencil.entries(e).offset)
        if (stencilRight.level > stencilLeft.stencilFieldSelection.stencil.level) {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : IR_Expression) / 2 + leftOffset(d)
        } else {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : IR_Expression) + leftOffset(d)
        }

        val combOff = leftOffset
        ResolveCoordinates.replacement = rightOffset
        ResolveCoordinates.doUntilDoneStandalone(combOff)

        var combCoeff : IR_Expression = (re.coefficient * new IR_FieldAccess(fieldSel, stencilFieldIdx))
        SimplifyStrategy.doUntilDoneStandalone(combOff)
        SimplifyStrategy.doUntilDoneStandalone(combCoeff)
        val addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
        if (addToEntry.isDefined) {
          combCoeff += addToEntry.get.coefficient
          SimplifyStrategy.doUntilDoneStandalone(combCoeff)
          addToEntry.get.coefficient = combCoeff
        } else entries += new StencilEntry(combOff, combCoeff)
      }
    }

    StencilAccess(Stencil(stencilLeft.stencilFieldSelection.stencil.identifier + "_" + stencilRight.identifier, stencilLeft.stencilFieldSelection.stencil.level, entries))
  }
}

//////////////////////////// SIMD Expressions \\\\\\\\\\\\\\\\\\\\\\\\\\\\

case class SIMD_LoadExpression(var mem : IR_Expression, val aligned : Boolean) extends IR_Expression {
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

case class SIMD_ExtractScalarExpression(var expr : IR_Expression, var index : Int) extends IR_Expression {
  override def datatype = expr.datatype
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

case class SIMD_ConcShift(var left : IR_VariableAccess, var right : IR_VariableAccess, val offset : Int) extends IR_Expression {
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

case class SIMD_NegateExpression(var vect : IR_Expression) extends IR_Expression {
  override def datatype = vect.datatype
  override def prettyprint(out : PpStream) : Unit = {
    val (prec, ts) = if (Knowledge.useDblPrecision) ('d', "d") else ('s', "")
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_xor_p" << prec << '(' << vect << ", _mm_set1_p" << prec << "(-0.0))"
      case "AVX" | "AVX2" => out << "_mm256_xor_p" << prec << '(' << vect << ", _mm256_set1_p" << prec << "(-0.0))"
      case "AVX512"       => out << "_mm512_xor_p" << prec << '(' << vect << ", _mm512_set1_p" << prec << "(-0.0))"
      case "IMCI"         => out << "_mm512_sub_p" << prec << "((__m512" << ts << ") 0, " << vect << ")" // TODO: is there a more efficient version?
      case "QPX"          => out << "vec_neg(" << vect << ')'
      case "NEON"         => out << "vnegq_f32(" << vect << ')'
    }
  }
}

case class SIMD_AdditionExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = GetResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_add_p" << prec
      case "AVX" | "AVX2"    => out << "_mm256_add_p" << prec
      case "AVX512" | "IMCI" => out << "_mm512_add_p" << prec
      case "QPX"             => out << "vec_add"
      case "NEON"            => out << "vaddq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_SubtractionExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = GetResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_sub_p" << prec
      case "AVX" | "AVX2"    => out << "_mm256_sub_p" << prec
      case "AVX512" | "IMCI" => out << "_mm512_sub_p" << prec
      case "QPX"             => out << "vec_sub"
      case "NEON"            => out << "vsubq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_MultiplicationExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = GetResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_mul_p" << prec
      case "AVX" | "AVX2"    => out << "_mm256_mul_p" << prec
      case "AVX512" | "IMCI" => out << "_mm512_mul_p" << prec
      case "QPX"             => out << "vec_mul"
      case "NEON"            => out << "vmulq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_MultiplyAddExpression(
    var factor1 : IR_Expression,
    var factor2 : IR_Expression,
    var summand : IR_Expression) extends IR_Expression {
  override def datatype = GetResultingDatatype(factor1.datatype, factor2.datatype, summand.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    FusedPrinterHelper.prettyprint(out, factor1, factor2, summand, "add")
  }
}

case class SIMD_MultiplySubExpression(
    var factor1 : IR_Expression,
    var factor2 : IR_Expression,
    var summand : IR_Expression) extends IR_Expression {
  override def datatype = GetResultingDatatype(factor1.datatype, factor2.datatype, summand.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    FusedPrinterHelper.prettyprint(out, factor1, factor2, summand, "sub")
  }
}

private object FusedPrinterHelper {
  def prettyprint(out : PpStream, factor1 : IR_Expression, factor2 : IR_Expression, summand : IR_Expression, addSub : String) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_" << addSub << "_p" << prec << "(_mm_mul_p" << prec << '(' << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX"             => out << "_mm256_" << addSub << "_p" << prec << "(_mm256_mul_p" << prec << '(' << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX2"            => out << "_mm256_fm" << addSub << "_p" << prec << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "AVX512" | "IMCI" => out << "_mm512_fm" << addSub << "_p" << prec << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "QPX"             => out << "vec_m" << addSub << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "NEON"            =>
        if (addSub == "add")
          out << "vmlaq_f32(" << summand << ", " << factor1 << ", " << factor2 << ')' // use unfused for compatibility with gcc 4.7 and older
        else // vmlsq_f32(a,b,c) is a-b*c and not a*b-c; thanks ARM  -.-
          out << "vnegq_f32(vmlsq_f32(" << summand << ", " << factor1 << ", " << factor2 << "))"
    }
  }
}

case class SIMD_DivisionExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = GetResultingDatatype(left.datatype, right.datatype)
  // FIXME
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_div_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_div_p" << prec
      case "AVX512"       => out << "_mm512_div_p" << prec
      case "IMCI"         => throw new InternalError("not yet supported...") // TODO: support it! but there is no div :(
      case "QPX"          => out << "vec_swdiv_nochk" // double precision division performed here, single precision would also be possible... what's better?
      case "NEON"         => out << "vdivq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_MinimumExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = GetResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "QPX") // TODO: export function
      out << "vec_sel(" << right << ", " << left << ", vec_cmplt(" << left << ", " << right << "))" // vec_sel selects the second if the third represents true...
    else {
      val prec = if (Knowledge.useDblPrecision) 'd' else 's'
      Platform.simd_instructionSet match {
        case "SSE3"         => out << "_mm_min_p" << prec
        case "AVX" | "AVX2" => out << "_mm256_min_p" << prec
        case "AVX512"       => out << "_mm512_min_p" << prec
        case "IMCI"         => out << "_mm512_gmin_p" << prec
        case "NEON"         => out << "vmin_f32"
      }
      out << '(' << left << ", " << right << ')'
    }
  }
}

case class SIMD_MaximumExpression(var left : IR_Expression, var right : IR_Expression) extends IR_Expression {
  override def datatype = GetResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "QPX") // TODO: export function
      out << "vec_sel(" << right << ", " << left << ", vec_cmpgt(" << left << ", " << right << "))" // vec_sel selects the second if the third represents true...
    else {
      val prec = if (Knowledge.useDblPrecision) 'd' else 's'
      Platform.simd_instructionSet match {
        case "SSE3"         => out << "_mm_max_p" << prec
        case "AVX" | "AVX2" => out << "_mm256_max_p" << prec
        case "AVX512"       => out << "_mm512_max_p" << prec
        case "IMCI"         => out << "_mm512_gmax_p" << prec
        case "NEON"         => out << "vmax_f32"
      }
      out << '(' << left << ", " << right << ')'
    }
  }
}

case class SIMD_Scalar2VectorExpression(var scalar : IR_Expression) extends IR_Expression {
  override def datatype = new IR_VectorDatatype(scalar.datatype, 1)
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_set1_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_set1_p" << prec
      case "AVX512"       => out << "_mm512_set1_p" << prec
      case "IMCI"         => out << "_mm512_set_1to" << Platform.simd_vectorSize << "_p" << prec
      case "QPX"          => out << "vec_splats"
      case "NEON"         => out << "vdupq_n_f32"
    }
    out << '(' << scalar << ')'
  }
}
