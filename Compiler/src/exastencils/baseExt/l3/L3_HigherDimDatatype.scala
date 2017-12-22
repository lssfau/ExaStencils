package exastencils.baseExt.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4._
import exastencils.prettyprinting.PpStream

/// L3_HigherDimensionalDatatype

trait L3_HigherDimDatatype extends L3_Datatype {
  // encapsulated data type
  def datatype : L3_Datatype

  override def resolveBaseDatatype : L3_Datatype = datatype.resolveBaseDatatype
}

/// L3_VectorDatatype

object L3_VectorDatatype {
  def apply(datatype : L3_Datatype, numElements : Int) = new L3_VectorDatatype(datatype, numElements, false)
}

case class L3_VectorDatatype(var datatype : L3_Datatype, var numElements : Int, var isRow : Boolean) extends L3_HigherDimDatatype {
  override def prettyprint(out : PpStream) = {
    out << (if (isRow) "RowVector" else "ColumnVector")
    out << "<" << datatype << ',' << numElements << '>'
  }

  override def progress = ProgressLocation(L4_VectorDatatype(datatype.progress, numElements, isRow))

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
  override def resolveDeclType : L3_Datatype = this
  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
  override def typicalByteSize = numElements * datatype.typicalByteSize
}

/// L3_MatrixDatatype

case class L3_MatrixDatatype(var datatype : L3_Datatype, var numRows : Int, var numColumns : Int) extends L3_HigherDimDatatype {
  override def prettyprint(out : PpStream) : Unit = out << "Matrix<" << datatype << ',' << numRows << ',' << numColumns << '>'
  override def progress = ProgressLocation(L4_MatrixDatatype(datatype.progress, numRows, numColumns))

  override def dimensionality : Int = 3 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numRows, numColumns) ++ datatype.getSizeArray
  override def resolveDeclType : L3_Datatype = this
  override def resolveFlattendSize : Int = numRows * numColumns * datatype.resolveFlattendSize
  override def typicalByteSize = numRows * numColumns * datatype.typicalByteSize
}
