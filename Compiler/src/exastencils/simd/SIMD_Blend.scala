package exastencils.simd

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream


case class SIMD_Blendv(var a : IR_Expression, var b : IR_Expression, var mask : IR_Expression) extends SIMD_Expression {
  override def datatype : IR_Datatype = SIMD_RealDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_or_pd( _mm_and_pd(" << b << "," << mask << "), _mm_andnot_pd(" << mask << "," << a << ") )"
      case "AVX" | "AVX2" => out << "_mm256_blendv_p" << prec << '(' << a << "," << b << "," << mask << ")"
      case "AVX512"       => out << "_mm512_mask_blend_p" << prec << '(' << mask << "," << a << "," << b << ")"
      case "NEON"         => Logger.error("Currently unsupported")
      case "IMCI"         => Logger.error("Currently unsupported")
      case "QPX"          => Logger.error("Currently unsupported")
    }
  }
}
