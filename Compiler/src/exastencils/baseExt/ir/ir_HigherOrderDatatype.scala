package exastencils.baseExt.ir

import exastencils.base.ir._
import exastencils.datastructures.ir
import exastencils.prettyprinting.PpStream

/// higher order data types

trait IR_HigherOrderDatatype extends IR_Datatype {
  def datatype : IR_Datatype
  // encapsulated data type
  override def resolveBaseDatatype : IR_Datatype = datatype.resolveBaseDatatype
}

case class IR_ArrayDatatype(datatype : IR_Datatype, numElements : Int) extends IR_HigherOrderDatatype {
  override def prettyprint(out : PpStream) : Unit = out << this.resolveDeclType << this.resolveDeclPostscript
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = datatype.resolveDeclType
  override def resolveDeclPostscript : String = s"[$numElements]" + datatype.resolveDeclPostscript
  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
  override def typicalByteSize = numElements * datatype.typicalByteSize
}

case class IR_ArrayDatatype_VS(datatype : IR_Datatype, numElements : ir.Expression) extends IR_HigherOrderDatatype {
  override def prettyprint(out : PpStream) : Unit = out << this.resolveDeclType << this.resolveDeclPostscript
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = ???
  override def resolveDeclType : IR_Datatype = datatype.resolveDeclType
  override def resolveDeclPostscript : String = s"[$numElements]" + datatype.resolveDeclPostscript
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

// TODO: use after successful integration:
//case class VectorDatatype(var datatype : IR_Datatype, var numElements : Int, var isRow : Option[Boolean]) extends HigherOrderDatatype {
//  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
//  override def prettyprint(out : PpStream) : Unit = {
//    if (isRow.getOrElse(true)) out << "Matrix<" << datatype << ",1," << numElements << '>'
//    else out << "Matrix<" << datatype << ',' << numElements << ",1>"
//  }
//
//  override def dimensionality : Int = 1 + datatype.dimensionality
//  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
//  override def resolveDeclType : IR_Datatype = this
//  override def resolveDeclPostscript : String = ""
//  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
//  override def typicalByteSize = numElements * datatype.typicalByteSize
//}
case class IR_VectorDatatype(var datatype : IR_Datatype, var numElements : Int, var isRow : Option[Boolean]) extends IR_HigherOrderDatatype {
  override def prettyprint(out : PpStream) : Unit = {
    if (isRow.getOrElse(true)) out << "Matrix<" << datatype << ",1," << numElements << '>'
    else out << "Matrix<" << datatype << ',' << numElements << ",1>"
  }
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
  override def typicalByteSize = numElements * datatype.typicalByteSize
}

// TODO: use after successful integration:
//case class MatrixDatatype(var datatype : IR_Datatype, var numRows : Int, var numColumns : Int) extends HigherOrderDatatype {
//  override def prettyprint(out : PpStream) : Unit = out << "Matrix<" << datatype << ',' << numRows << ',' << numColumns << '>'
//  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
//
//  override def dimensionality : Int = 2 + datatype.dimensionality
//  override def getSizeArray : Array[Int] = Array(numRows, numColumns) ++ datatype.getSizeArray
//  override def resolveDeclType : IR_Datatype = this
//  override def resolveDeclPostscript : String = ""
//  override def resolveFlattendSize : Int = numRows * numColumns * datatype.resolveFlattendSize
//  override def typicalByteSize = numRows * numColumns * datatype.typicalByteSize
//}
case class IR_MatrixDatatype(var datatype : IR_Datatype, var numRows : Int, var numColumns : Int) extends IR_HigherOrderDatatype {
  override def prettyprint(out : PpStream) : Unit = out << "Matrix<" << datatype << ',' << numRows << ',' << numColumns << '>'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 2 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numRows, numColumns) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = numRows * numColumns * datatype.resolveFlattendSize
  override def typicalByteSize = numRows * numColumns * datatype.typicalByteSize
}
