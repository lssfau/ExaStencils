package exastencils.baseExt.l4

import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

trait L4_HigherOrderDatatype extends L4_Datatype {
  // encapsulated data type
  def datatype : L4_Datatype

  override def resolveBaseDatatype : L4_Datatype = datatype.resolveBaseDatatype
}

case class L4_ArrayDatatype(datatype : L4_Datatype, numElements : Int) extends L4_HigherOrderDatatype {
  override def prettyprint(out : PpStream) = { out << "Array<" << datatype << "><" << numElements << '>' }
  override def progress = IR_ArrayDatatype(datatype.progress, numElements)

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
  override def resolveDeclType : L4_Datatype = datatype.resolveDeclType
  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
  override def typicalByteSize = numElements * datatype.typicalByteSize
}

case class L4_ArrayDatatype_VS(datatype : L4_Datatype, numElements : L4_Expression) extends L4_HigherOrderDatatype {
  override def prettyprint(out : PpStream) = { out << "Array[" << datatype << "][" << numElements << ']' }
  override def progress = IR_ArrayDatatype_VS(datatype.progress, numElements.progress)

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = ???
  override def resolveDeclType : L4_Datatype = datatype.resolveDeclType
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case class L4_VectorDatatype(var datatype : L4_Datatype, var numElements : Int, var isRow : Option[Boolean]) extends L4_HigherOrderDatatype {
  override def prettyprint(out : PpStream) = {
    if (isRow.getOrElse(true))
      out << "Vector"
    else
      out << "ColumnVector"
    out << "<" << datatype << ',' << numElements << '>'
  }
  override def progress = {
    if (Knowledge.experimental_internalHighDimTypes) {
      IR_MatrixDatatype(datatype.progress, if (isRow.getOrElse(true)) numElements else 1, if (isRow.getOrElse(true)) 1 else numElements)
    } else {
      IR_VectorDatatype(datatype.progress, numElements, isRow)
    }
  }

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
  override def typicalByteSize = numElements * datatype.typicalByteSize
}

case class L4_MatrixDatatype(var datatype : L4_Datatype, var numRows : Int, var numColumns : Int) extends L4_HigherOrderDatatype {
  override def prettyprint(out : PpStream) : Unit = out << "Matrix<" << datatype << ',' << numRows << ',' << numColumns << '>'
  override def progress = IR_MatrixDatatype(datatype.progress, numRows, numColumns)

  override def dimensionality : Int = 2 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numRows, numColumns) ++ datatype.getSizeArray
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = numRows * numColumns * datatype.resolveFlattendSize
  override def typicalByteSize = numRows * numColumns * datatype.typicalByteSize
}
