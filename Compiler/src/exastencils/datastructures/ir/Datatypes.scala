package exastencils.datastructures.ir

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.knowledge._
import exastencils.prettyprinting._

/// SIMD data types

trait SIMDDatatype extends IR_Datatype {
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

case object SIMD_RealDatatype extends SIMDDatatype {
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

/// helper functions and objects

object GetResultingDatatype {
  def apply(a : Option[IR_Datatype], b : Option[IR_Datatype]) : Option[IR_Datatype] = {
    if (a.isDefined && b.isEmpty) return None
    if (a.isEmpty && b.isDefined) return None
    if (a.isEmpty && b.isEmpty) return None

    a.get match {
      case IR_IntegerDatatype => b.get match {
        case IR_IntegerDatatype          => Some(IR_IntegerDatatype)
        case IR_RealDatatype             => Some(IR_RealDatatype)
        case IR_StringDatatype           => Some(IR_StringDatatype)
        case IR_CharDatatype             => Some(IR_IntegerDatatype)
        case IR_ArrayDatatype(dt, l)     => Some(IR_ArrayDatatype(dt, l))
        case IR_ComplexDatatype(dt)      => Some(IR_ComplexDatatype(dt))
        case IR_VectorDatatype(dt, l, r) => Some(IR_VectorDatatype(dt, l, r))
        case IR_MatrixDatatype(dt, m, n) => Some(IR_MatrixDatatype(dt, m, n))
      }
      case IR_RealDatatype => b.get match {
        case IR_IntegerDatatype          => Some(IR_RealDatatype)
        case IR_RealDatatype             => Some(IR_RealDatatype)
        case IR_StringDatatype           => Some(IR_StringDatatype)
        case IR_CharDatatype             => Some(IR_RealDatatype)
        case IR_ArrayDatatype(dt, l)     => Some(IR_ArrayDatatype(GetResultingDatatype(Some(dt), a).getOrElse(dt), l))
        case IR_ComplexDatatype(dt)      => Some(IR_ComplexDatatype(GetResultingDatatype(Some(dt), a).getOrElse(dt)))
        case IR_VectorDatatype(dt, l, r) => Some(IR_VectorDatatype(GetResultingDatatype(Some(dt), a).getOrElse(dt), l, r))
        case IR_MatrixDatatype(dt, m, n) => Some(IR_MatrixDatatype(GetResultingDatatype(Some(dt), a).getOrElse(dt), m, n))
      }
      case IR_StringDatatype => b.get match {
        case IR_IntegerDatatype          => Some(IR_StringDatatype)
        case IR_RealDatatype             => Some(IR_StringDatatype)
        case IR_StringDatatype           => Some(IR_StringDatatype)
        case IR_CharDatatype             => Some(IR_StringDatatype)
        case IR_ArrayDatatype(dt, l)     => Some(IR_StringDatatype)
        case IR_ComplexDatatype(dt)      => Some(IR_StringDatatype)
        case IR_VectorDatatype(dt, l, r) => Some(IR_StringDatatype)
        case IR_MatrixDatatype(dt, m, n) => Some(IR_StringDatatype)
      }
      case IR_CharDatatype => b.get match {
        case IR_IntegerDatatype          => Some(IR_IntegerDatatype)
        case IR_RealDatatype             => Some(IR_RealDatatype)
        case IR_StringDatatype           => Some(IR_StringDatatype)
        case IR_ArrayDatatype(dt, l)     => Some(IR_ArrayDatatype(dt, l))
        case IR_ComplexDatatype(dt)      => Some(IR_ComplexDatatype(dt))
        case IR_VectorDatatype(dt, l, r) => Some(IR_VectorDatatype(dt, l, r))
        case IR_MatrixDatatype(dt, m, n) => Some(IR_MatrixDatatype(dt, m, n))
      }
      case IR_ArrayDatatype(dt, l)     => Some(IR_ArrayDatatype(GetResultingDatatype(Some(dt), b).getOrElse(dt), l))
      case IR_ComplexDatatype(dt)      => Some(IR_ComplexDatatype(GetResultingDatatype(Some(dt), b).getOrElse(dt)))
      case IR_VectorDatatype(dt, l, r) => Some(IR_VectorDatatype(GetResultingDatatype(Some(dt), b).getOrElse(dt), l, r))
      case IR_MatrixDatatype(dt, m, n) => Some(IR_MatrixDatatype(GetResultingDatatype(Some(dt), b).getOrElse(dt), m, n))
    }
  }
}

object GetResultingDatatype2 {
  def apply(a : IR_Datatype, b : IR_Datatype) : IR_Datatype = {
    if (a eq IR_UnitDatatype) return IR_UnitDatatype
    if (b eq IR_UnitDatatype) return IR_UnitDatatype

    a match {
      case IR_IntegerDatatype => b match {
        case IR_IntegerDatatype          => IR_IntegerDatatype
        case IR_RealDatatype             => IR_RealDatatype
        case IR_FloatDatatype            => IR_FloatDatatype
        case IR_DoubleDatatype           => IR_DoubleDatatype
        case IR_StringDatatype           => IR_StringDatatype
        case IR_CharDatatype             => IR_IntegerDatatype
        case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(dt, l)
        case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(dt)
        case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(dt, l, r)
        case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(dt, m, n)
      }
      case IR_RealDatatype => b match {
        case IR_IntegerDatatype          => IR_RealDatatype
        case IR_RealDatatype             => IR_RealDatatype
        case IR_FloatDatatype            => IR_RealDatatype // Real is _at least_ Float
        case IR_DoubleDatatype           => IR_DoubleDatatype
        case IR_StringDatatype           => IR_StringDatatype
        case IR_CharDatatype             => IR_RealDatatype
        case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(GetResultingDatatype2(dt, a), l)
        case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(GetResultingDatatype2(dt, a))
        case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(GetResultingDatatype2(dt, a), l, r)
        case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(GetResultingDatatype2(dt, a), m, n)
      }
      case IR_FloatDatatype => b match {
        case IR_IntegerDatatype          => IR_RealDatatype
        case IR_RealDatatype             => IR_RealDatatype // Real is _at least_ Float
        case IR_FloatDatatype            => IR_FloatDatatype
        case IR_DoubleDatatype           => IR_DoubleDatatype
        case IR_StringDatatype           => IR_StringDatatype
        case IR_CharDatatype             => IR_RealDatatype
        case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(GetResultingDatatype2(dt, a), l)
        case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(GetResultingDatatype2(dt, a))
        case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(GetResultingDatatype2(dt, a), l, r)
        case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(GetResultingDatatype2(dt, a), m, n)
      }
      case IR_DoubleDatatype => b match {
        case IR_IntegerDatatype          => IR_DoubleDatatype
        case IR_RealDatatype             => IR_DoubleDatatype
        case IR_FloatDatatype            => IR_DoubleDatatype
        case IR_DoubleDatatype           => IR_DoubleDatatype
        case IR_StringDatatype           => IR_StringDatatype
        case IR_CharDatatype             => IR_DoubleDatatype
        case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(GetResultingDatatype2(dt, a), l)
        case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(GetResultingDatatype2(dt, a))
        case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(GetResultingDatatype2(dt, a), l, r)
        case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(GetResultingDatatype2(dt, a), m, n)
      }
      case IR_StringDatatype => b match {
        case IR_IntegerDatatype          => IR_StringDatatype
        case IR_RealDatatype             => IR_StringDatatype
        case IR_FloatDatatype            => IR_StringDatatype
        case IR_DoubleDatatype           => IR_StringDatatype
        case IR_StringDatatype           => IR_StringDatatype
        case IR_CharDatatype             => IR_StringDatatype
        case IR_ArrayDatatype(dt, l)     => IR_StringDatatype
        case IR_ComplexDatatype(dt)      => IR_StringDatatype
        case IR_VectorDatatype(dt, l, r) => IR_StringDatatype
        case IR_MatrixDatatype(dt, m, n) => IR_StringDatatype
      }
      case IR_CharDatatype => b match {
        case IR_IntegerDatatype          => IR_IntegerDatatype
        case IR_RealDatatype             => IR_RealDatatype
        case IR_FloatDatatype            => IR_FloatDatatype
        case IR_DoubleDatatype           => IR_DoubleDatatype
        case IR_StringDatatype           => IR_StringDatatype
        case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(dt, l)
        case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(dt)
        case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(dt, l, r)
        case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(dt, m, n)
      }
      case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(GetResultingDatatype2(dt, a), l)
      case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(GetResultingDatatype2(dt, a))
      case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(GetResultingDatatype2(dt, a), l, r)
      case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(GetResultingDatatype2(dt, a), m, n)
      case x : IR_SpecialDatatype      => x
    }
  }
  def apply(a : IR_Datatype, b : IR_Datatype, c : IR_Datatype) : IR_Datatype = apply(apply(a, b), c)
}
