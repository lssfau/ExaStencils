package exastencils.datastructures.ir

import exastencils.datastructures._
import exastencils.knowledge._
import exastencils.prettyprinting._

trait Datatype extends Node with PrettyPrintable {
  def prettyprint_mpi : String

  def dimensionality : Int
  def getSizeArray : Array[Int]
  def resolveUnderlyingDatatype : Datatype
  def resolvePostscript : String // TODO: rename
  def resolveFlattendSize : Int
  def typicalByteSize : Int
}

/// special data types

case class SpecialDatatype(typeName : String) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << typeName
  override def prettyprint_mpi = typeName

  // unknown
  override def dimensionality : Int = ???
  override def getSizeArray : Array[Int] = ???
  override def resolveUnderlyingDatatype : Datatype = this
  override def resolvePostscript : String = ""
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case object UnitDatatype extends Datatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "void"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveUnderlyingDatatype : Datatype = this
  override def resolvePostscript : String = ""
  override def resolveFlattendSize : Int = 0
  override def typicalByteSize = 0
}

/// scalar data types

trait ScalarDatatype extends Datatype {
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveUnderlyingDatatype : Datatype = this
  override def resolvePostscript : String = ""
  override def resolveFlattendSize : Int = 1
}

case object BooleanDatatype extends ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "bool"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
  override def typicalByteSize = 1
}

case object IntegerDatatype extends ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "int"
  override def prettyprint_mpi = "MPI_INT"
  override def typicalByteSize = 4
}

case object RealDatatype extends ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = {
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

case object CharDatatype extends ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "char"
  override def prettyprint_mpi = "MPI::CHAR"
  override def typicalByteSize = 1
}

/// higher order data types

trait HigherOrderDatatype extends Datatype {
  def datatype : Datatype // encapsulated data type
  override def resolveUnderlyingDatatype : Datatype = datatype.resolveUnderlyingDatatype
}

// FIXME: in the following classes, rename size to numElements to make intention clearer

case class ArrayDatatype(datatype : Datatype, size : Int) extends HigherOrderDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '[' << size << ']'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(size) ++ datatype.getSizeArray
  override def resolvePostscript : String = datatype.resolvePostscript + s"[$size]"
  override def resolveFlattendSize : Int = size * datatype.resolveFlattendSize
  override def typicalByteSize = size * datatype.typicalByteSize
}

case class ArrayDatatype_VS(datatype : Datatype, size : Expression) extends HigherOrderDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '[' << size << ']'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = ???
  override def resolvePostscript : String = datatype.resolvePostscript + s"[$size]"
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case class VectorDatatype(var datatype : Datatype, var size : Int, var isRow : Option[Boolean]) extends HigherOrderDatatype {
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
  override def prettyprint(out : PpStream) : Unit = {
    if (isRow.getOrElse(true)) out << "Matrix<" << datatype << ",1," << size << '>'
    else out << "Matrix<" << datatype << ',' << size << ",1>"
  }

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(size) ++ datatype.getSizeArray
  override def resolvePostscript : String = ""
  override def resolveFlattendSize : Int = size * datatype.resolveFlattendSize
  override def typicalByteSize = size * datatype.typicalByteSize
}

case class MatrixDatatype(datatype : Datatype, sizeM : Int, sizeN : Int) extends HigherOrderDatatype {
  override def prettyprint(out : PpStream) : Unit = out << "Matrix<" << datatype << ',' << sizeM << ',' << sizeN << '>'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 2 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(sizeM, sizeN) ++ datatype.getSizeArray
  override def resolvePostscript : String = ""
  override def resolveFlattendSize : Int = sizeM * sizeN * datatype.resolveFlattendSize
  override def typicalByteSize = sizeM * sizeN * datatype.typicalByteSize
}

/// data type modifiers

trait DatatypeModifier extends Datatype {
  def datatype : Datatype

  override def dimensionality : Int = datatype.dimensionality
  override def getSizeArray : Array[Int] = datatype.getSizeArray
  override def resolveUnderlyingDatatype : Datatype = datatype.resolveUnderlyingDatatype
  override def resolvePostscript : String = datatype.resolvePostscript
  override def resolveFlattendSize : Int = datatype.resolveFlattendSize
  override def typicalByteSize = datatype.typicalByteSize
}

case class VolatileDatatype(datatype : Datatype) extends DatatypeModifier {
  override def prettyprint(out : PpStream) : Unit = out << "volatile " << datatype
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

/// references and pointers

trait IndirectionDatatype extends Datatype {
  def datatype : Datatype

  override def dimensionality : Int = datatype.dimensionality
  override def getSizeArray : Array[Int] = datatype.getSizeArray
  override def resolveUnderlyingDatatype : Datatype = datatype.resolveUnderlyingDatatype
  override def resolvePostscript : String = datatype.resolvePostscript
  override def resolveFlattendSize : Int = datatype.resolveFlattendSize
}

trait PointerLikeDatatype extends IndirectionDatatype {
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveUnderlyingDatatype : Datatype = this
  override def resolvePostscript : String = ""
  override def resolveFlattendSize : Int = 1

  override def typicalByteSize = if (Knowledge.hw_64bit) 8 else 4
}

trait ReferenceLikeDatatype extends IndirectionDatatype {
  override def dimensionality : Int = datatype.dimensionality
  override def getSizeArray : Array[Int] = datatype.getSizeArray
  override def resolveUnderlyingDatatype : Datatype = datatype.resolveUnderlyingDatatype
  override def resolvePostscript : String = datatype.resolvePostscript
  override def resolveFlattendSize : Int = datatype.resolveFlattendSize
  override def typicalByteSize = datatype.typicalByteSize
}

case class PointerDatatype(datatype : Datatype) extends PointerLikeDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '*'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class ConstPointerDatatype(datatype : Datatype) extends PointerLikeDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << "* const"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class ReferenceDatatype(datatype : Datatype) extends ReferenceLikeDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '&'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

// add const ref, etc here if required

/// standard data types

case object StringDatatype extends Datatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "std::string"
  override def prettyprint_mpi = "MPI::CHAR"

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveUnderlyingDatatype : Datatype = this
  override def resolvePostscript : String = ""
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case class ComplexDatatype(datatype : Datatype) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << "std::complex<" << datatype << '>'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  // TODO: treat like a vec2 or like a struct?

  override def dimensionality : Int = 0 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array() ++ datatype.getSizeArray
  override def resolveUnderlyingDatatype : Datatype = datatype.resolveUnderlyingDatatype
  override def resolvePostscript : String = datatype.resolvePostscript
  override def resolveFlattendSize : Int = 1 * datatype.resolveFlattendSize
  override def typicalByteSize = 2 * datatype.typicalByteSize
}

/// SIMD data types

trait SIMDDatatype extends Datatype {
  def datatype : ScalarDatatype

  // TODO: currently treated similar to a vector - correct?

  override def dimensionality : Int = 1
  override def getSizeArray : Array[Int] = Array(Knowledge.simd_vectorSize)
  override def resolveUnderlyingDatatype : Datatype = this
  override def resolvePostscript : String = ""
  override def resolveFlattendSize : Int = Knowledge.simd_vectorSize
  override def typicalByteSize = Knowledge.simd_vectorSize * datatype.typicalByteSize
}

case object SIMD_RealDatatype extends SIMDDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def datatype : ScalarDatatype = RealDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val suffix = if (Knowledge.useDblPrecision) "d" else ""
    Knowledge.simd_instructionSet match {
      case "SSE3"            => out << "__m128" << suffix
      case "AVX" | "AVX2"    => out << "__m256" << suffix
      case "AVX512" | "IMCI" => out << "__m512" << suffix
      case "QPX"             => out << "vector4double" // no suffix
      case "NEON"            => out << "float32x4_t" // FIXME: only single precision until now
    }
  }
  override def prettyprint_mpi = "INVALID DATATYPE: " + this.prettyprint()
}

/// helper functions and objects

object GetResultingDatatype {
  def apply(a : Option[Datatype], b : Option[Datatype]) : Option[Datatype] = {
    if (a.isDefined && b.isEmpty) return None
    if (a.isEmpty && b.isDefined) return None
    if (a.isEmpty && b.isEmpty) return None

    a.get match {
      case IntegerDatatype => b.get match {
        case IntegerDatatype          => Some(IntegerDatatype)
        case RealDatatype             => Some(RealDatatype)
        case StringDatatype           => Some(StringDatatype)
        case CharDatatype             => Some(IntegerDatatype)
        case ArrayDatatype(dt, l)     => Some(ArrayDatatype(dt, l))
        case ComplexDatatype(dt)      => Some(ComplexDatatype(dt))
        case VectorDatatype(dt, l, r) => Some(VectorDatatype(dt, l, r))
        case MatrixDatatype(dt, m, n) => Some(MatrixDatatype(dt, m, n))
      }
      case RealDatatype => b.get match {
        case IntegerDatatype          => Some(RealDatatype)
        case RealDatatype             => Some(RealDatatype)
        case StringDatatype           => Some(StringDatatype)
        case CharDatatype             => Some(RealDatatype)
        case ArrayDatatype(dt, l)     => Some(ArrayDatatype(GetResultingDatatype(Some(dt), a).getOrElse(dt), l))
        case ComplexDatatype(dt)      => Some(ComplexDatatype(GetResultingDatatype(Some(dt), a).getOrElse(dt)))
        case VectorDatatype(dt, l, r) => Some(VectorDatatype(GetResultingDatatype(Some(dt), a).getOrElse(dt), l, r))
        case MatrixDatatype(dt, m, n) => Some(MatrixDatatype(GetResultingDatatype(Some(dt), a).getOrElse(dt), m, n))
      }
      case StringDatatype => b.get match {
        case IntegerDatatype          => Some(StringDatatype)
        case RealDatatype             => Some(StringDatatype)
        case StringDatatype           => Some(StringDatatype)
        case CharDatatype             => Some(StringDatatype)
        case ArrayDatatype(dt, l)     => Some(StringDatatype)
        case ComplexDatatype(dt)      => Some(StringDatatype)
        case VectorDatatype(dt, l, r) => Some(StringDatatype)
        case MatrixDatatype(dt, m, n) => Some(StringDatatype)
      }
      case CharDatatype => b.get match {
        case IntegerDatatype          => Some(IntegerDatatype)
        case RealDatatype             => Some(RealDatatype)
        case StringDatatype           => Some(StringDatatype)
        case ArrayDatatype(dt, l)     => Some(ArrayDatatype(dt, l))
        case ComplexDatatype(dt)      => Some(ComplexDatatype(dt))
        case VectorDatatype(dt, l, r) => Some(VectorDatatype(dt, l, r))
        case MatrixDatatype(dt, m, n) => Some(MatrixDatatype(dt, m, n))
      }
      case ArrayDatatype(dt, l)     => Some(ArrayDatatype(GetResultingDatatype(Some(dt), b).getOrElse(dt), l))
      case ComplexDatatype(dt)      => Some(ComplexDatatype(GetResultingDatatype(Some(dt), b).getOrElse(dt)))
      case VectorDatatype(dt, l, r) => Some(VectorDatatype(GetResultingDatatype(Some(dt), b).getOrElse(dt), l, r))
      case MatrixDatatype(dt, m, n) => Some(MatrixDatatype(GetResultingDatatype(Some(dt), b).getOrElse(dt), m, n))
    }
  }
}
