package exastencils.baseExt.ir

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

trait IR_ContainerDatatypeSTL extends IR_HigherDimensionalDatatype {
  override def prettyprint_mpi : String = s"INVALID DATATYPE: " + this.prettyprint()
}

case class IR_StdVectorDatatype(var datatype: IR_Datatype) extends IR_ContainerDatatypeSTL {

  override def dimensionality : Int = 1 + this.datatype.dimensionality
  override def getSizeArray : Array[Int] = ???
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???

  override def prettyprint(out : PpStream) : Unit = out << "std::vector< " << this.datatype << " >"
}

case class IR_StdArrayDatatype(var datatype: IR_Datatype, numElements : Int) extends IR_ContainerDatatypeSTL {

  override def dimensionality : Int = 1 + this.datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ this.datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = numElements * this.datatype.resolveFlattendSize
  override def typicalByteSize = numElements * this.datatype.typicalByteSize

  override def prettyprint(out : PpStream) : Unit = out << "std::array<" << this.datatype << ", " << numElements << ">"
}