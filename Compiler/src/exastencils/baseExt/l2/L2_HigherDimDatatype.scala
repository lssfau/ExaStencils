package exastencils.baseExt.l2

import exastencils.base.l2._
import exastencils.baseExt.l3._
import exastencils.prettyprinting.PpStream

/// L2_HigherDimensionalDatatype

trait L2_HigherDimDatatype extends L2_Datatype {
  // encapsulated data type
  def datatype : L2_Datatype

  override def resolveBaseDatatype : L2_Datatype = datatype.resolveBaseDatatype
}

/// L2_VectorDatatype

object L2_VectorDatatype {
  def apply(datatype : L2_Datatype, numElements : Int) = new L2_VectorDatatype(datatype, numElements, false)
}

case class L2_VectorDatatype(var datatype : L2_Datatype, var numElements : Int, var isRow : Boolean) extends L2_HigherDimDatatype {
  override def prettyprint(out : PpStream) = {
    out << (if (isRow) "RowVector" else "ColumnVector")
    out << "<" << datatype << ',' << numElements << '>'
  }

  override def progress = L3_VectorDatatype(datatype.progress, numElements, isRow)

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
  override def resolveDeclType : L2_Datatype = this
  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
  override def typicalByteSize = numElements * datatype.typicalByteSize
}

/// L2_MatrixDatatype

case class L2_MatrixDatatype(var datatype : L2_Datatype, var numRows : Int, var numColumns : Int) extends L2_HigherDimDatatype {
  override def prettyprint(out : PpStream) : Unit = out << "Matrix<" << datatype << ',' << numRows << ',' << numColumns << '>'
  override def progress = L3_MatrixDatatype(datatype.progress, numRows, numColumns)

  override def dimensionality : Int = 2 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numRows, numColumns) ++ datatype.getSizeArray
  override def resolveDeclType : L2_Datatype = this
  override def resolveFlattendSize : Int = numRows * numColumns * datatype.resolveFlattendSize
  override def typicalByteSize = numRows * numColumns * datatype.typicalByteSize
}
