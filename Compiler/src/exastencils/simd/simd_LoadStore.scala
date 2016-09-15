package exastencils.simd

import exastencils.base.ir._
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream

/// IR_SIMD_Store

case class IR_SIMD_Store(var mem : IR_Expression, var value : IR_Expression, var aligned : Boolean) extends IR_Statement {
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
