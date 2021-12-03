package exastencils.simd

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.optimization.ir.VectorizationException
import exastencils.prettyprinting.PpStream


case class SIMD_Blendv(var a : IR_Expression, var b : IR_Expression, var mask : IR_Expression) extends SIMD_Expression {
  override def datatype : IR_Datatype = SIMD_RealDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    val precBits = if (Knowledge.useDblPrecision) 64 else 32
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_or_p" << prec << "( _mm_and_p" << prec << "(" << b << "," << mask << "), _mm_andnot_p" << prec << "(" << mask << "," << a << ") )"
      case "AVX" | "AVX2" => out << "_mm256_blendv_p" << prec << '(' << a << "," << b << "," << mask << ")"
      case "AVX512"       => out << "_mm512_mask_blend_p" << prec << '(' << mask << "," << a << "," << b << ")"
      // TODO: test
      case "NEON"         => out <<  "vbslq_f" << precBits << "(" << mask << ", " << a << ", " << b << ")"
      // TODO
      case "IMCI"         => new VectorizationException("SIMD_Blendv: Currently unsupported for " + Platform.simd_instructionSet)
      case "QPX"          => new VectorizationException("SIMD_Blendv: Currently unsupported for " + Platform.simd_instructionSet)
    }
  }
}
