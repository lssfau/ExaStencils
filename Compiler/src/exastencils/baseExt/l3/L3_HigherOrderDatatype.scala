package exastencils.baseExt.l3

import exastencils.base.l3._
import exastencils.baseExt.l4._
import exastencils.prettyprinting.PpStream

trait L3_HigherOrderDatatype extends L3_Datatype {
  // encapsulated data type
  def datatype : L3_Datatype

  override def resolveBaseDatatype : L3_Datatype = datatype.resolveBaseDatatype
}

case class L3_ArrayDatatype(datatype : L3_Datatype, numElements : Int) extends L3_HigherOrderDatatype {
  override def prettyprint(out : PpStream) = { out << "Array[" << datatype << "][" << numElements << ']' }
  override def progress = L4_ArrayDatatype(datatype.progress, numElements)

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
  override def resolveDeclType : L3_Datatype = datatype.resolveDeclType
  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
  override def typicalByteSize = numElements * datatype.typicalByteSize
}
