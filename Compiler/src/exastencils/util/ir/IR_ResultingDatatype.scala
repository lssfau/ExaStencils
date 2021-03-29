//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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
        case IR_TensorDatatype1(dt, d)      => IR_TensorDatatype1(dt, d)
        case IR_TensorDatatype2(dt, d)      => IR_TensorDatatype2(dt, d)
        case IR_TensorDatatypeN(dt, d, 1)   => IR_TensorDatatype1(dt, d)
        case IR_TensorDatatypeN(dt, d, 2)   => IR_TensorDatatype2(dt, d)
        case IR_TensorDatatypeN(dt, d, ord) => IR_TensorDatatypeN(dt, d, ord)
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
        case IR_TensorDatatype1(dt, d)      => IR_TensorDatatype1(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatype2(dt, d)      => IR_TensorDatatype2(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatypeN(dt, d, 1)   => IR_TensorDatatype1(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatypeN(dt, d, 2)   => IR_TensorDatatype2(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatypeN(dt, d, ord) => IR_TensorDatatypeN(IR_ResultingDatatype(dt, a), d, ord)
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
        case IR_TensorDatatype1(dt, d)      => IR_TensorDatatype1(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatype2(dt, d)      => IR_TensorDatatype2(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatypeN(dt, d, 1)   => IR_TensorDatatype1(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatypeN(dt, d, 2)   => IR_TensorDatatype2(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatypeN(dt, d, ord) => IR_TensorDatatypeN(IR_ResultingDatatype(dt, a), d, ord)
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
        case IR_TensorDatatype1(dt, d)      => IR_TensorDatatype1(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatype2(dt, d)      => IR_TensorDatatype2(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatypeN(dt, d, 1)   => IR_TensorDatatype1(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatypeN(dt, d, 2)   => IR_TensorDatatype2(IR_ResultingDatatype(dt, a), d)
        case IR_TensorDatatypeN(dt, d, ord) => IR_TensorDatatypeN(IR_ResultingDatatype(dt, a), d, ord)
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
        case IR_TensorDatatype1(dt, d)      => IR_StringDatatype
        case IR_TensorDatatype2(dt, d)      => IR_StringDatatype
        case IR_TensorDatatypeN(dt, d, ord) => IR_StringDatatype
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
        case IR_TensorDatatype1(dt, d)      => IR_TensorDatatype1(dt, d)
        case IR_TensorDatatype2(dt, d)      => IR_TensorDatatype2(dt, d)
        case IR_TensorDatatypeN(dt, d, 1)   => IR_TensorDatatype1(dt, d)
        case IR_TensorDatatypeN(dt, d, 2)   => IR_TensorDatatype2(dt, d)
        case IR_TensorDatatypeN(dt, d, ord) => IR_TensorDatatypeN(dt, d, ord)
      }
      case IR_ArrayDatatype(dt, l)     => IR_ArrayDatatype(IR_ResultingDatatype(dt, a), l)
      case IR_ComplexDatatype(dt)      => IR_ComplexDatatype(IR_ResultingDatatype(dt, a))
      case IR_VectorDatatype(dt, l, r) => IR_VectorDatatype(IR_ResultingDatatype(dt, a), l, r)
      case IR_MatrixDatatype(dt, m, n) => IR_MatrixDatatype(IR_ResultingDatatype(dt, a.resolveBaseDatatype), m, n)
      case IR_TensorDatatype1(dt, d)      => IR_TensorDatatype1(IR_ResultingDatatype(dt, a.resolveBaseDatatype), d)
      case IR_TensorDatatype2(dt, d)      => IR_TensorDatatype2(IR_ResultingDatatype(dt, a.resolveBaseDatatype), d)
      case IR_TensorDatatypeN(dt, d, 1)   => IR_TensorDatatype1(IR_ResultingDatatype(dt, a.resolveBaseDatatype), d)
      case IR_TensorDatatypeN(dt, d, 2)   => IR_TensorDatatype2(IR_ResultingDatatype(dt, a.resolveBaseDatatype), d)
      case IR_TensorDatatypeN(dt, d, ord) => IR_TensorDatatypeN(IR_ResultingDatatype(dt, a.resolveBaseDatatype), d, ord)
      case x : IR_SpecialDatatype      => x
    }
  }
  def apply(a : IR_Datatype, b : IR_Datatype, c : IR_Datatype) : IR_Datatype = apply(apply(a, b), c)
}
