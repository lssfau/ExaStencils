package exastencils.simd

import exastencils.base.ir._
import exastencils.config._
import exastencils.prettyprinting.PpStream

trait IR_SIMD_Datatype extends IR_Datatype {
  def datatype : IR_ScalarDatatype

  // TODO: currently treated similar to a vector - correct?

  override def dimensionality : Int = 1
  override def getSizeArray : Array[Int] = Array(Platform.simd_vectorSize)
  override def resolveBaseDatatype : IR_Datatype = datatype
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = Platform.simd_vectorSize
  override def typicalByteSize = Platform.simd_vectorSize * datatype.typicalByteSize
}

case object IR_SIMD_RealDatatype extends IR_SIMD_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def datatype : IR_ScalarDatatype = IR_RealDatatype

  override def prettyprint(out : PpStream) : Unit = {
    val suffix = if (Knowledge.useDblPrecision) "d" else ""
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "__m128" << suffix
      case "AVX" | "AVX2"    => out << "__m256" << suffix
      case "AVX512" | "IMCI" => out << "__m512" << suffix
      case "QPX"             => out << "vector4double" // no suffix
      case "NEON"            => out << "float32x4_t" // FIXME: only single precision until now
    }
  }

  override def prettyprint_mpi = "INVALID DATATYPE: " + this.prettyprint()
}
