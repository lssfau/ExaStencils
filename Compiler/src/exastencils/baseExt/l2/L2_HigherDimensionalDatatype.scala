package exastencils.baseExt.l2

import exastencils.base.l2._
import exastencils.baseExt.l3._
import exastencils.prettyprinting.PpStream

trait L2_HigherDimensionalDatatype extends L2_Datatype {
  // encapsulated data type
  def datatype : L2_Datatype

  override def resolveBaseDatatype : L2_Datatype = datatype.resolveBaseDatatype
}

case class L2_ArrayDatatype(datatype : L2_Datatype, numElements : Int) extends L2_HigherDimensionalDatatype {
  override def prettyprint(out : PpStream) = { out << "Array[" << datatype << "][" << numElements << ']' }
  override def progress = L3_ArrayDatatype(datatype.progress, numElements)

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
  override def resolveDeclType : L2_Datatype = datatype.resolveDeclType
  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
  override def typicalByteSize = numElements * datatype.typicalByteSize
}
