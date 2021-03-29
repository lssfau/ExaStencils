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

package exastencils.baseExt.ir

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.prettyprinting.PpStream

/// higher order data types

trait IR_HigherDimensionalDatatype extends IR_Datatype {
  def datatype : IR_Datatype
  // encapsulated data type
  override def resolveBaseDatatype : IR_Datatype = datatype.resolveBaseDatatype
}

trait IR_HasTypeAlias {
  def aliasFor : String
}

case class IR_ArrayDatatype(datatype : IR_Datatype, numElements : Int) extends IR_HigherDimensionalDatatype {
  override def prettyprint(out : PpStream) : Unit = out << this.resolveDeclType << this.resolveDeclPostscript
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = datatype.resolveDeclType
  override def resolveDeclPostscript : String = s"[$numElements]" + datatype.resolveDeclPostscript
  override def resolveFlattendSize : Int = numElements * datatype.resolveFlattendSize
  override def typicalByteSize = numElements * datatype.typicalByteSize
}

case class IR_ArrayDatatype_VS(datatype : IR_Datatype, numElements : IR_Expression) extends IR_HigherDimensionalDatatype {
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
case class IR_VectorDatatype(var datatype : IR_Datatype, var size : Int, var isRow : Option[Boolean] = Some(true)) extends IR_HigherDimensionalDatatype {
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

case class IR_MatrixDatatype(var datatype : IR_Datatype, var sizeM : Int, var sizeN : Int) extends IR_HigherDimensionalDatatype with IR_HasTypeAlias {
  override def prettyprint(out : PpStream) : Unit = {
    out << "__matrix_"
    if (datatype.isInstanceOf[IR_ComplexDatatype]) out << "complex_" << datatype.resolveBaseDatatype else out << datatype
    out << '_' << sizeM << "_" << sizeN << "_t"
  }
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 2 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(sizeM, sizeN) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = this.datatype.resolveDeclType
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = sizeM * sizeN * datatype.resolveFlattendSize
  override def typicalByteSize = sizeM * sizeN * datatype.typicalByteSize

  override def aliasFor = datatype.prettyprint + '[' + this.resolveFlattendSize + ']'
  def transposed = IR_MatrixDatatype(datatype, sizeN, sizeM)
}

abstract class IR_TensorDatatype(datatype : IR_Datatype, dims : Int) extends IR_HigherDimensionalDatatype with IR_HasTypeAlias {
  override def prettyprint(out : PpStream)
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality: Int
  override def getSizeArray: Array[Int]
  override def resolveDeclType: IR_Datatype
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize: Int
  override def typicalByteSize: Int

  override def aliasFor = datatype.prettyprint + '[' + this.resolveFlattendSize + ']'
}

case class IR_TensorDatatype1(datatype : IR_Datatype, dims : Int) extends IR_TensorDatatype(datatype, dims) {
  override def prettyprint(out : PpStream) : Unit = out << "__tensor1_" << this.dims.toString << "_" << datatype << "_t"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(dims) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = this.datatype.resolveDeclType
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = dims * datatype.resolveFlattendSize
  override def typicalByteSize = dims * datatype.typicalByteSize

  override def aliasFor = datatype.prettyprint + '[' + this.resolveFlattendSize + ']'
}

case class IR_TensorDatatype2(datatype : IR_Datatype, dims : Int) extends IR_TensorDatatype(datatype, dims) {
  override def prettyprint(out : PpStream) : Unit = out << "__tensor2_" << this.dims.toString << "_" << datatype << "_t"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 2 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(dims * dims) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = this.datatype.resolveDeclType
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = dims * dims * datatype.resolveFlattendSize
  override def typicalByteSize = dims * dims * datatype.typicalByteSize

  override def aliasFor = datatype.prettyprint + '[' + this.resolveFlattendSize + ']'
  def transposed = IR_TensorDatatype2(datatype, dims)
}

case class IR_TensorDatatypeN(datatype : IR_Datatype, dims : Int, var order : Int) extends IR_TensorDatatype(datatype, dims) {
  override def prettyprint(out : PpStream) : Unit = out << "__tensorN_" << this.order.toString << "_" << this.datatype << "_t"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = order + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(scala.math.pow(dims, order).toInt) ++ datatype.getSizeArray
  override def resolveDeclType : IR_Datatype = this.datatype.resolveDeclType
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = scala.math.pow(dims, order).toInt * this.datatype.resolveFlattendSize
  override def typicalByteSize = scala.math.pow(dims, order).toInt * datatype.typicalByteSize

  override def aliasFor = datatype.prettyprint + '[' + this.resolveFlattendSize.toString  + ']'
  def transposed = IR_TensorDatatypeN(datatype, dims, order)
}

object IR_HACK_TypeAliases extends DefaultStrategy("Register type aliases") {
  // FIXME remove this hack for a better data layout
  var global : IR_GlobalCollection = null

  override def apply(applyAtNode : Option[Node]) = {
    global = IR_GlobalCollection.get
    super.apply(applyAtNode)
  }

  this += new Transformation("do", {
    case t : IR_HasTypeAlias => global.registerTypeAlias(t.asInstanceOf[IR_Datatype], t.aliasFor); t
  })
}
