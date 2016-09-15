package exastencils.simd

import exastencils.base.ir._
import exastencils.datastructures.ir.GetResultingDatatype
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream

/// IR_SIMD_Addition

case class IR_SIMD_Addition(var left : IR_Expression, var right : IR_Expression) extends IR_SIMD_Expression {
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

/// IR_SIMD_Subtraction

case class IR_SIMD_Subtraction(var left : IR_Expression, var right : IR_Expression) extends IR_SIMD_Expression {
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

/// IR_SIMD_Multiplication

case class IR_SIMD_Multiplication(var left : IR_Expression, var right : IR_Expression) extends IR_SIMD_Expression {
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

/// IR_SIMD_Division

case class IR_SIMD_Division(var left : IR_Expression, var right : IR_Expression) extends IR_SIMD_Expression {
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

/// IR_SIMD_Minimum

case class IR_SIMD_Minimum(var left : IR_Expression, var right : IR_Expression) extends IR_SIMD_Expression {
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

/// IR_SIMD_Maximum

case class IR_SIMD_Maximum(var left : IR_Expression, var right : IR_Expression) extends IR_SIMD_Expression {
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
