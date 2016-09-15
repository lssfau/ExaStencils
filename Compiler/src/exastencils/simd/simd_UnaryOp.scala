package exastencils.simd

import exastencils.base.ir.IR_Expression
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream

/// IR_SIMD_Negate

case class IR_SIMD_Negate(var vect : IR_Expression) extends IR_SIMD_Expression {
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
