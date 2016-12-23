package exastencils.util.ir

import exastencils.base.ir._
import exastencils.baseExt.ir._

object IR_ResultingDatatype {
  def apply(a : IR_Datatype, b : IR_Datatype) : IR_Datatype = {
    if (a eq IR_UnitDatatype) return IR_UnitDatatype
    if (b eq IR_UnitDatatype) return IR_UnitDatatype
    if (a eq IR_UnknownDatatype) return b
    if (b eq IR_UnknownDatatype) return a

    a match {
      case IR_IntegerDatatype          => b match {
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
      case IR_RealDatatype             => b match {
        case IR_IntegerDatatype          => IR_RealDatatype
        case IR_RealDatatype             => IR_RealDatatype
        case IR_FloatDatatype            => IR_RealDatatype // Real is _at least_ Float
        case IR_DoubleDatatype           => IR_DoubleDatatype
        case IR_StringDatatype           => IR_StringDatatype
        case IR_CharDatatype             => IR_RealDatatype
        case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(IR_ResultingDatatype(dt, a), l)
        case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(IR_ResultingDatatype(dt, a))
        case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(IR_ResultingDatatype(dt, a), l, r)
        case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(IR_ResultingDatatype(dt, a), m, n)
      }
      case IR_FloatDatatype            => b match {
        case IR_IntegerDatatype          => IR_RealDatatype
        case IR_RealDatatype             => IR_RealDatatype // Real is _at least_ Float
        case IR_FloatDatatype            => IR_FloatDatatype
        case IR_DoubleDatatype           => IR_DoubleDatatype
        case IR_StringDatatype           => IR_StringDatatype
        case IR_CharDatatype             => IR_RealDatatype
        case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(IR_ResultingDatatype(dt, a), l)
        case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(IR_ResultingDatatype(dt, a))
        case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(IR_ResultingDatatype(dt, a), l, r)
        case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(IR_ResultingDatatype(dt, a), m, n)
      }
      case IR_DoubleDatatype           => b match {
        case IR_IntegerDatatype          => IR_DoubleDatatype
        case IR_RealDatatype             => IR_DoubleDatatype
        case IR_FloatDatatype            => IR_DoubleDatatype
        case IR_DoubleDatatype           => IR_DoubleDatatype
        case IR_StringDatatype           => IR_StringDatatype
        case IR_CharDatatype             => IR_DoubleDatatype
        case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(IR_ResultingDatatype(dt, a), l)
        case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(IR_ResultingDatatype(dt, a))
        case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(IR_ResultingDatatype(dt, a), l, r)
        case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(IR_ResultingDatatype(dt, a), m, n)
      }
      case IR_StringDatatype           => b match {
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
      case IR_CharDatatype             => b match {
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
      case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(IR_ResultingDatatype(dt, a), l)
      case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(IR_ResultingDatatype(dt, a))
      case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(IR_ResultingDatatype(dt, a), l, r)
      case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(IR_ResultingDatatype(dt, a), m, n)
      case x : IR_SpecialDatatype      => x
    }
  }
  def apply(a : IR_Datatype, b : IR_Datatype, c : IR_Datatype) : IR_Datatype = apply(apply(a, b), c)
}
