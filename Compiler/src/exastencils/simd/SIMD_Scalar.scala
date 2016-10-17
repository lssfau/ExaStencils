package exastencils.simd

import exastencils.base.ir.IR_Expression
import exastencils.baseExt.ir.IR_VectorDatatype
import exastencils.config._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// IR_SIMD_ExtractScalar

case class IR_SIMD_ExtractScalar(var expr : IR_Expression, var index : Int) extends IR_SIMD_Expression {
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

/// IR_SIMD_Scalar2Vector

case class IR_SIMD_Scalar2Vector(var scalar : IR_Expression) extends IR_SIMD_Expression {
  override def datatype = IR_VectorDatatype(scalar.datatype, 1)
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
