package exastencils.baseExt.ir

import exastencils.base.ir._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.prettyprinting.PpStream

/// higher order data types

trait IR_HigherOrderDatatype extends IR_Datatype {
  def datatype : IR_Datatype
  // encapsulated data type
  override def resolveBaseDatatype : IR_Datatype = datatype.resolveBaseDatatype
}

trait IR_HasTypeAlias {
  def aliasFor : String
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

case class IR_MatrixDatatype(var datatype : IR_Datatype, var sizeM : Int, var sizeN : Int) extends IR_HigherOrderDatatype with IR_HasTypeAlias {
  override def prettyprint(out : PpStream) : Unit = if (exastencils.config.Knowledge.experimental_internalHighDimTypes) {
    out << "__matrix_" << datatype << '_' << sizeM << "_" << sizeN << "_t"
  } else {
    out << "Matrix<" << datatype << ',' << sizeM << ',' << sizeN << '>'
  }
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 2 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(sizeM, sizeN) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = if (exastencils.config.Knowledge.experimental_internalHighDimTypes) this.datatype.resolveDeclType else this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = sizeM * sizeN * datatype.resolveFlattendSize
  override def typicalByteSize = sizeM * sizeN * datatype.typicalByteSize

  override def aliasFor = datatype.prettyprint + '[' + this.resolveFlattendSize + ']'
}

object IR_HACK_TypeAliases extends DefaultStrategy("Register type aliases") {
  // FIXME remove this hack for a better data layout
  val global = IR_GlobalCollection.get

  this += new Transformation("do", {
    case t : IR_HasTypeAlias => global.registerTypeAlias(t.asInstanceOf[IR_Datatype], t.aliasFor); t
  })
}

