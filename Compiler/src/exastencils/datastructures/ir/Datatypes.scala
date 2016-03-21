package exastencils.datastructures.ir

import exastencils.datastructures._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.logger.Logger

trait Datatype extends Node with PrettyPrintable {
  def prettyprint_mpi : String

  def dimensionality : Int
  def getSizeArray : Array[Int]
  def resolveBaseDatatype : Datatype
  def resolveDeclType : Datatype
  def resolveDeclPostscript : String
  def resolveFlattendSize : Int
  def typicalByteSize : Int
  def isNumeric : Boolean
}

/// special data types

case class SpecialDatatype(typeName : String) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << typeName
  override def prettyprint_mpi = typeName

  // unknown
  override def dimensionality : Int = ???
  override def getSizeArray : Array[Int] = ???
  override def resolveBaseDatatype : Datatype = this
  override def resolveDeclType : Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
  override def isNumeric = ???
}

case object UnitDatatype extends Datatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "void"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : Datatype = this
  override def resolveDeclType : Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = 0
  override def typicalByteSize = 0
  override def isNumeric = true
}

/// scalar data types

trait ScalarDatatype extends Datatype {
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : Datatype = this
  override def resolveDeclType : Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = 1
  override def isNumeric = true
}

case object BooleanDatatype extends ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "bool"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
  override def typicalByteSize = 1
  override def isNumeric = false
}

case object IntegerDatatype extends ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "int"
  override def prettyprint_mpi = "MPI_INT"
  override def typicalByteSize = 4
  override def isNumeric = true
}

case object RealDatatype extends ScalarDatatype {
  var printedDepWarn = false

  exastencils.core.Duplicate.registerConstant(this)
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
  override def isNumeric = true
}

case object FloatDatatype extends ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "float"
  override def prettyprint_mpi = "MPI_FLOAT"
  override def typicalByteSize = 4
}

case object DoubleDatatype extends ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "double"
  override def prettyprint_mpi = "MPI_DOUBLE"
  override def typicalByteSize = 8
}

case object CharDatatype extends ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "char"
  override def prettyprint_mpi = "MPI::CHAR"
  override def typicalByteSize = 1
  override def isNumeric = false
}

/// higher-dimensional data types

trait HigherDimensionalDatatype extends Datatype {
  def datatype : Datatype // encapsulated data type
  override def resolveBaseDatatype : Datatype = datatype.resolveBaseDatatype
}

// FIXME: in the following classes, rename size to numElements to make intention clearer

case class ArrayDatatype(override val datatype : Datatype, size : Int) extends HigherDimensionalDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '[' << size << ']'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(size) ++ datatype.getSizeArray
  override def resolveDeclType : Datatype = datatype.resolveDeclType
  override def resolveDeclPostscript : String = datatype.resolveDeclPostscript + s"[$size]"
  override def resolveFlattendSize : Int = size * datatype.resolveFlattendSize
  override def typicalByteSize = size * datatype.typicalByteSize
  override def isNumeric = datatype.isNumeric
}

case class ArrayDatatype_VS(override val datatype : Datatype, size : Expression) extends HigherDimensionalDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '[' << size << ']'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = ???
  override def resolveDeclType : Datatype = datatype.resolveDeclType
  override def resolveDeclPostscript : String = datatype.resolveDeclPostscript + s"[$size]"
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
  override def isNumeric = datatype.isNumeric
}

case class VectorDatatype(var datatype : Datatype, var size : Int, var isRow : Boolean = true) extends HigherDimensionalDatatype {
  override def prettyprint(out : PpStream) : Unit = {
    if (isRow) out << "INVALID DATATYPE: Matrix<" << datatype << ",1," << size << '>'
    else out << "INVALID DATATYPE: Matrix<" << datatype << ',' << size << ",1>"
  }
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(size) ++ datatype.getSizeArray
  override def resolveDeclType : Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = size * datatype.resolveFlattendSize
  override def typicalByteSize = size * datatype.typicalByteSize
  override def isNumeric = datatype.isNumeric
}

case class MatrixDatatype(var datatype : Datatype, var sizeM : Int, var sizeN : Int) extends HigherDimensionalDatatype {
  override def prettyprint(out : PpStream) : Unit = out << "INVALID DATATYPE: Matrix<" << datatype << ',' << sizeM << ',' << sizeN << '>'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  override def dimensionality : Int = 2 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(sizeM, sizeN) ++ datatype.getSizeArray
  override def resolveDeclType : Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = sizeM * sizeN * datatype.resolveFlattendSize
  override def typicalByteSize = sizeM * sizeN * datatype.typicalByteSize
  override def isNumeric = datatype.isNumeric
}

/// data type modifiers

trait DatatypeModifier extends Datatype {
  def datatype : Datatype

  override def dimensionality : Int = datatype.dimensionality
  override def getSizeArray : Array[Int] = datatype.getSizeArray
  override def resolveBaseDatatype : Datatype = datatype.resolveBaseDatatype
  override def resolveDeclType : Datatype = datatype.resolveDeclType
  override def resolveDeclPostscript : String = datatype.resolveDeclPostscript
  override def resolveFlattendSize : Int = datatype.resolveFlattendSize
  override def typicalByteSize = datatype.typicalByteSize
  override def isNumeric = datatype.isNumeric
}

case class VolatileDatatype(override val datatype : Datatype) extends DatatypeModifier {
  override def prettyprint(out : PpStream) : Unit = out << "volatile " << datatype
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

/// references and pointers

trait IndirectionDatatype extends Datatype {
  def datatype : Datatype

  override def dimensionality : Int = datatype.dimensionality
  override def getSizeArray : Array[Int] = datatype.getSizeArray
  override def resolveBaseDatatype : Datatype = datatype.resolveBaseDatatype
  override def resolveDeclType : Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = datatype.resolveFlattendSize
  override def isNumeric = datatype.isNumeric
}

trait PointerLikeDatatype extends IndirectionDatatype {
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveFlattendSize : Int = 1

  override def typicalByteSize = if (Platform.hw_64bit) 8 else 4
}

trait ReferenceLikeDatatype extends IndirectionDatatype {
  override def dimensionality : Int = datatype.dimensionality
  override def getSizeArray : Array[Int] = datatype.getSizeArray
  override def resolveDeclPostscript : String = datatype.resolveDeclPostscript
  override def resolveFlattendSize : Int = datatype.resolveFlattendSize
  override def typicalByteSize = datatype.typicalByteSize
}

case class PointerDatatype(override val datatype : Datatype) extends PointerLikeDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '*'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class ConstPointerDatatype(override val datatype : Datatype) extends PointerLikeDatatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << "* const"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class ReferenceDatatype(override val datatype : Datatype) extends ReferenceLikeDatatype {
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
  override def resolveBaseDatatype : Datatype = CharDatatype
  override def resolveDeclType : Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
  override def isNumeric = false
}

case class ComplexDatatype(datatype : Datatype) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << "std::complex<" << datatype << '>'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()

  // TODO: treat like a vec2 or like a struct?

  override def dimensionality : Int = 0 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array() ++ datatype.getSizeArray
  override def resolveBaseDatatype : Datatype = datatype.resolveBaseDatatype
  override def resolveDeclType : Datatype = this
  override def resolveDeclPostscript : String = datatype.resolveDeclPostscript
  override def resolveFlattendSize : Int = 1 * datatype.resolveFlattendSize
  override def typicalByteSize = 2 * datatype.typicalByteSize
  override def isNumeric = datatype.isNumeric
}

/// SIMD data types

trait SIMDDatatype extends Datatype {
  def datatype : ScalarDatatype

  // TODO: currently treated similar to a vector - correct?

  override def dimensionality : Int = 1
  override def getSizeArray : Array[Int] = Array(Platform.simd_vectorSize)
  override def resolveBaseDatatype : Datatype = datatype
  override def resolveDeclType : Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = Platform.simd_vectorSize
  override def typicalByteSize = Platform.simd_vectorSize * datatype.typicalByteSize
}

case object SIMD_RealDatatype extends SIMDDatatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def datatype : ScalarDatatype = RealDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val suffix = if (Knowledge.useDblPrecision) "d" else ""
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "__m128" << suffix
      case "AVX" | "AVX2"    => out << "__m256" << suffix
      case "AVX512" | "IMCI" => out << "__m512" << suffix
      case "QPX"             => out << "vector4double" // no suffix
      case "NEON"            => out << "float32x4_t" // FIXME: only single precision until now
    }
  }
  override def prettyprint_mpi = "INVALID DATATYPE: " + this.prettyprint()
  override def isNumeric = datatype.isNumeric
}

/// helper functions and objects

object GetResultingDatatype {
  def apply(operator : String, a : Option[Datatype], b : Option[Datatype]) : Option[Datatype] = {
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
        case ArrayDatatype(dt, l)     => Some(ArrayDatatype(GetResultingDatatype(operator, Some(dt), a).getOrElse(dt), l))
        case ComplexDatatype(dt)      => Some(ComplexDatatype(GetResultingDatatype(operator, Some(dt), a).getOrElse(dt)))
        case VectorDatatype(dt, l, r) => Some(VectorDatatype(GetResultingDatatype(operator, Some(dt), a).getOrElse(dt), l, r))
        case MatrixDatatype(dt, m, n) => Some(MatrixDatatype(GetResultingDatatype(operator, Some(dt), a).getOrElse(dt), m, n))
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
      case ArrayDatatype(dt, l) => Some(ArrayDatatype(GetResultingDatatype(operator, Some(dt), b).getOrElse(dt), l))
      case ComplexDatatype(dt)  => Some(ComplexDatatype(GetResultingDatatype(operator, Some(dt), b).getOrElse(dt)))

      case VectorDatatype(dta, la, rowa) => b.get match {
        case VectorDatatype(dtb, lb, rowb) => {
          if (la != lb) Logger.error("Vector lengths have to match")
          if (operator == "*") {
            if (!rowa && rowb) Some(MatrixDatatype(GetResultingDatatype("*", dta, dtb).get, la, lb)) /* ColumnVec * RowVec => Matrix */
            else if (rowa && !rowb) Some(GetResultingDatatype("*", dta, dtb).get) /* RowVec * ColumnVec => Scalar */
            else Logger.error(s"""Could not get resulting type for operation '$operator' for $a and $b""") /* RowVec * RowVec or ColumnVec * ColumnVec */
          } else {
            if (!operator.startsWith(".") && rowa != rowb) Logger.error("Vector types (row or column) have to match!")
            Some(VectorDatatype(GetResultingDatatype(operator, dta, dtb).get, la, rowa))
          }
        }
        case MatrixDatatype(dtb, mb, nb) => {
          if (operator == "*") {
            if (la != mb) Logger.error("Vector length must match matrix size M")
            if (nb == 1) GetResultingDatatype(operator, dta, dtb) // is scalar product
            else Some(MatrixDatatype(GetResultingDatatype("*", dta, dtb).get, 1, nb))
          } else {
            Logger.error(s"""Invalid operation '$operator' for types $a and $b""")
          }
        }
        case _ => GetResultingDatatype(operator, b, a)
      }

      case MatrixDatatype(dta, ma, na) => b.get match {
        case MatrixDatatype(dtb, mb, nb) => {
          if (operator == "*") {
            if (na != mb) Logger.error(s"""Invalid matrix sizes for operator '$operator'""")
            Some(MatrixDatatype(GetResultingDatatype("*", dta, dtb).get, ma, nb))
          } else {
            if (na != ma && nb != mb) Logger.error(s"""Invalid matrix sizes for operator '$operator'""")
            Some(MatrixDatatype(GetResultingDatatype(operator, dta, dtb).get, ma, na))
          }
        }
        case VectorDatatype(dtb, lb, rowb) => {
          if (operator == "*") {
            if (rowb) Logger.error(s"""Invalid operation '$operator' for types $a and $b""")
            Some(VectorDatatype(GetResultingDatatype("*", dta, dtb).get, ma, false))
          } else {
            Logger.error(s"""Invalid operation '$operator' for types $a and $b""")
          }
        }
      }
    }
  }

  def apply(operator : String, a : Datatype, b : Datatype) : Option[Datatype] = apply(operator, Some(a), Some(b))
  def apply(operator : String, a : Option[Datatype], b : Datatype) : Option[Datatype] = apply(operator, a, Some(b))
  def apply(operator : String, a : Datatype, b : Option[Datatype]) : Option[Datatype] = apply(operator, Some(a), b)
}
