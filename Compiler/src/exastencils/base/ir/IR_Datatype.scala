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

package exastencils.base.ir

import exastencils.config._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// IR_Datatype

trait IR_Datatype extends IR_Node with PrettyPrintable {
  def prettyprint_mpi : String

  def dimensionality : Int
  def getSizeArray : Array[Int]
  def resolveBaseDatatype : IR_Datatype
  def resolveDeclType : IR_Datatype
  def resolveDeclPostscript : String
  def resolveFlattendSize : Int
  def typicalByteSize : Int
}

/// IR_UnitDatatype

case object IR_UnknownDatatype extends IR_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "???"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : IR_Datatype = this
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = 0
  override def typicalByteSize = 0
}

/// special data types

case class IR_SpecialDatatype(typeName : String) extends IR_Datatype {
  override def prettyprint(out : PpStream) : Unit = out << typeName
  override def prettyprint_mpi = typeName

  // unknown
  override def dimensionality : Int = ???
  override def getSizeArray : Array[Int] = ???
  override def resolveBaseDatatype : IR_Datatype = this
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case object IR_UnitDatatype extends IR_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "void"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : IR_Datatype = this
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = 0
  override def typicalByteSize = 0
}

/// scalar data types

trait IR_ScalarDatatype extends IR_Datatype {
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : IR_Datatype = this
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = 1
}

case object IR_BooleanDatatype extends IR_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "bool"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def typicalByteSize = 1
}

case object IR_IntegerDatatype extends IR_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "int"
  override def prettyprint_mpi = "MPI_INT"

  override def typicalByteSize = 4
}

case object IR_RealDatatype extends IR_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  var printedDepWarn = false

  override def prettyprint(out : PpStream) : Unit = {
    if (!printedDepWarn) {
      Logger.warn("RealDatatype is deprecated - please switch to FloatDatatype or DoubleDatatype")
      printedDepWarn = true
    }
    if (Knowledge.useDblPrecision)
      out << "double"
    else
      out << "float"
  }

  override def prettyprint_mpi = {
    if (Knowledge.useDblPrecision)
      "MPI_DOUBLE"
    else
      "MPI_FLOAT"
  }

  override def typicalByteSize = {
    if (Knowledge.useDblPrecision)
      8
    else
      4
  }
}

case object IR_FloatDatatype extends IR_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "float"
  override def prettyprint_mpi = "MPI_FLOAT"

  override def typicalByteSize = 4
}

case object IR_DoubleDatatype extends IR_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "double"
  override def prettyprint_mpi = "MPI_DOUBLE"

  override def typicalByteSize = 8
}

case object IR_CharDatatype extends IR_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "char"
  override def prettyprint_mpi = "MPI::CHAR"

  override def typicalByteSize = 1
}

/// standard data types

case object IR_StringDatatype extends IR_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "std::string"
  override def prettyprint_mpi = "MPI::CHAR"

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : IR_Datatype = IR_CharDatatype
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case class IR_ComplexDatatype(datatype : IR_Datatype) extends IR_Datatype {
  if (!Settings.additionalIncludes.contains("complex"))
    Settings.additionalIncludes += "complex"
  if (!Settings.makefile_additionalCFlags.contains("-std=c++14"))
    Settings.makefile_additionalCFlags += "-std=c++14"
  if (!Settings.additionalNamespaces.contains("std::complex_literals"))
    Settings.additionalNamespaces += "std::complex_literals"

  override def prettyprint(out : PpStream) : Unit = out << "std::complex<" << datatype << '>'
  override def prettyprint_mpi = "MPI::DOUBLE_COMPLEX"//s"INVALID DATATYPE: " + this.prettyprint()
  override def toString() : String = "std::complex<" + datatype + '>'

  // TODO: treat like a vec2 or like a struct?

  override def dimensionality : Int = 0 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array() ++ datatype.getSizeArray
  override def resolveBaseDatatype : IR_Datatype = datatype.resolveBaseDatatype
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = datatype.resolveDeclPostscript
  override def resolveFlattendSize : Int = 1 * datatype.resolveFlattendSize
  override def typicalByteSize = 2 * datatype.typicalByteSize
}

/// data type modifiers

trait IR_DatatypeModifier extends IR_Datatype {
  def datatype : IR_Datatype

  override def dimensionality : Int = datatype.dimensionality
  override def getSizeArray : Array[Int] = datatype.getSizeArray
  override def resolveBaseDatatype : IR_Datatype = datatype.resolveBaseDatatype
  override def resolveDeclType : IR_Datatype = datatype.resolveDeclType
  override def resolveDeclPostscript : String = datatype.resolveDeclPostscript
  override def resolveFlattendSize : Int = datatype.resolveFlattendSize
  override def typicalByteSize = datatype.typicalByteSize
}

case class IR_VolatileDatatype(override val datatype : IR_Datatype) extends IR_DatatypeModifier {
  override def prettyprint(out : PpStream) : Unit = out << "volatile " << datatype
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

/// references and pointers

trait IR_IndirectionDatatype extends IR_Datatype {
  def datatype : IR_Datatype

  override def dimensionality : Int = datatype.dimensionality
  override def getSizeArray : Array[Int] = datatype.getSizeArray
  override def resolveBaseDatatype : IR_Datatype = datatype.resolveBaseDatatype
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = datatype.resolveFlattendSize
}

trait IR_PointerLikeDatatype extends IR_IndirectionDatatype {
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveFlattendSize : Int = 1

  override def typicalByteSize = if (Platform.hw_64bit) 8 else 4
}

trait IR_ReferenceLikeDatatype extends IR_IndirectionDatatype {
  override def dimensionality : Int = datatype.dimensionality
  override def getSizeArray : Array[Int] = datatype.getSizeArray
  override def resolveDeclPostscript : String = datatype.resolveDeclPostscript
  override def resolveFlattendSize : Int = datatype.resolveFlattendSize
  override def typicalByteSize = datatype.typicalByteSize
}

// FIXME: IR_PointerDatatype(IR_ArrayDatatype(..)) results in incorrect C++ code...
case class IR_PointerDatatype(override val datatype : IR_Datatype) extends IR_PointerLikeDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '*'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

// FIXME: IR_ConstPointerDatatype(IR_ArrayDatatype(..)) results in incorrect C++ code...
case class IR_ConstPointerDatatype(override val datatype : IR_Datatype) extends IR_PointerLikeDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << "* const"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

// TODO: move to CUDA package
case class IR_CUDAConstPointerDatatype(override val datatype : IR_Datatype) extends IR_PointerLikeDatatype {
  override def prettyprint(out : PpStream) : Unit = out << "const " << datatype << "* __restrict__"
  override def prettyprint_mpi : String = s"INVALID DATATYPE: " + this.prettyprint()
}

case class IR_ReferenceDatatype(override val datatype : IR_Datatype) extends IR_ReferenceLikeDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '&'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

// add const ref, etc here if required
