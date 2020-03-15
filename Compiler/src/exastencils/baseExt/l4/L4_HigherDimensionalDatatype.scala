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

package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.prettyprinting.PpStream

/// L4_HigherDimensionalDatatype

trait L4_HigherDimensionalDatatype extends L4_Datatype {
  // encapsulated data type
  def datatype : L4_Datatype

  override def resolveBaseDatatype : L4_Datatype = datatype.resolveBaseDatatype
}

/// L4_VectorDatatype

object L4_VectorDatatype {
  def apply(datatype : L4_Datatype, numElements : Int) = new L4_VectorDatatype(datatype, numElements, false)
}

case class L4_VectorDatatype(var datatype : L4_Datatype, var numElements : Int, var isRow : Boolean) extends L4_HigherDimensionalDatatype {
  override def prettyprint(out : PpStream) = {
    out << (if (isRow) "RowVector" else "ColumnVector")
    out << "<" << datatype << ',' << numElements << '>'
  }

  override def progress = ProgressLocation {
    IR_MatrixDatatype(datatype.progress, if (isRow) 1 else numElements, if (isRow) numElements else 1)
  }

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
  override def typicalByteSize = numElements * datatype.typicalByteSize
}

/// L4_MatrixDatatype

case class L4_MatrixDatatype(var datatype : L4_Datatype, var numRows : Int, var numColumns : Int) extends L4_HigherDimensionalDatatype {
  override def prettyprint(out : PpStream) : Unit = out << "Matrix<" << datatype << ',' << numRows << ',' << numColumns << '>'
  override def progress = ProgressLocation(IR_MatrixDatatype(datatype.progress, numRows, numColumns))

  override def dimensionality : Int = 2 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numRows, numColumns) ++ datatype.getSizeArray
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = numRows * numColumns * datatype.resolveFlattendSize
  override def typicalByteSize = numRows * numColumns * datatype.typicalByteSize
}

/// L4_TensorDatatype

case class L4_TensorDatatype(var datatype : L4_Datatype, var dim : Int) extends L4_HigherDimensionalDatatype {
  override def prettyprint(out : PpStream) : Unit = out << "Matrix<" << datatype << ',' << dim << ',' << '>'
  override def progress = ProgressLocation(IR_TensorDatatype(datatype.progress, dim))

  override def dimensionality : Int = dim + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(3^dim) ++ datatype.getSizeArray
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = 3^dim * datatype.resolveFlattendSize
  override def typicalByteSize = 3^dim * datatype.typicalByteSize
}
