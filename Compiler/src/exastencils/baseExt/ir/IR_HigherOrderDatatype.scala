package exastencils.baseExt.ir

import exastencils.base.ir._
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

case class IR_ArrayDatatype_VS(datatype : IR_Datatype, numElements : IR_Expression) extends IR_HigherOrderDatatype {
  override def prettyprint(out : PpStream) : Unit = out << this.resolveDeclType << this.resolveDeclPostscript
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = ???
  override def resolveDeclType : IR_Datatype = datatype.resolveDeclType
  override def resolveDeclPostscript : String = s"[$numElements]" + datatype.resolveDeclPostscript
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

@deprecated("Switch to IR_MatrixDatatype")
case class IR_VectorDatatype(var datatype : IR_Datatype, var size : Int, var isRow : Option[Boolean] = Some(true)) extends IR_HigherOrderDatatype {
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
  override def prettyprint(out : PpStream) : Unit = {
    if (isRow.getOrElse(true)) out << "Matrix<" << datatype << ",1," << size << '>'
    else out << "Matrix<" << datatype << ',' << size << ",1>"
  }

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(size) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = size * datatype.resolveFlattendSize
  override def typicalByteSize = size * datatype.typicalByteSize
}

case class IR_MatrixDatatype(var datatype : IR_Datatype, var sizeM : Int, var sizeN : Int) extends IR_HigherOrderDatatype {
  override def prettyprint(out : PpStream) : Unit = if(exastencils.config.Knowledge.experimental_internalHighDimTypes) {
    out << datatype << '[' << sizeM << "][" << sizeN << ']'
  } else {
    out << "Matrix<" << datatype << ',' << sizeM << ',' << sizeN << '>'
  }
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 2 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(sizeM, sizeN) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = sizeM * sizeN * datatype.resolveFlattendSize
  override def typicalByteSize = sizeM * sizeN * datatype.typicalByteSize
}
