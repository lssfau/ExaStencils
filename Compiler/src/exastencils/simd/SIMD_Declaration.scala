package exastencils.simd

import exastencils.config._
import exastencils.prettyprinting.PpStream

/// SIMD_IncrementVectorDeclaration

/** Special declaration for a SIMD vector, which is initialized with the values 0, 1, ..., Knowledge.simd_vectorSize-1. */
case class SIMD_IncrementVectorDeclaration(var name : String, var incr : Long) extends SIMD_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << SIMD_RealDatatype << ' ' << name
    val is = Platform.simd_instructionSet
    is match {
      case "QPX" =>
        out << ";\n"
        out << "{\n"
        out << " double _a[4] __attribute__((aligned(32))) = { 0, " << incr << ", " << 2 * incr << ", " << 3 * incr << " };\n"
        out << ' ' << name << " = vec_lda(0, _a);\n"
        out << "}"

      case "SSE3" | "AVX" | "AVX2" | "AVX512" =>
        val bit = if (is == "SSE3") "" else if (is == "AVX512") "512" else "256"
        val prec = if (Knowledge.useDblPrecision) 'd' else 's'
        out << " = _mm" << bit << "_set_p" << prec << '('
        for (i <- Platform.simd_vectorSize - 1 to 1 by -1)
          out << i * incr << ", "
        out << "0);"

      case "IMCI" =>
        out << " (" << SIMD_RealDatatype << ") { 0"
        for (i <- 1 until Platform.simd_vectorSize)
          out << ", " << i * incr
        out << " };"

      case "NEON" =>
        out << ";\n"
        out << "{\n"
        out << " float _a[4] = { 0, " << incr << ", " << 2 * incr << ", " << 3 * incr << " };\n"
        out << ' ' << name << " = vld1q_f32(_a);\n"
        out << "}"
    }
  }
}
