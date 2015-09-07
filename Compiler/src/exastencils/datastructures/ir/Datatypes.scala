package exastencils.datastructures.ir

import exastencils.datastructures._
import exastencils.knowledge._
import exastencils.prettyprinting._

trait Datatype extends Node with PrettyPrintable {
  def prettyprint_mpi : String

  def resolveUnderlyingDatatype : Datatype = {
    this match {
      case ArrayDatatype(dt, size) => dt.resolveUnderlyingDatatype
      case dt                      => dt
    }
  }

  def resolvePostscript : String = {
    this match {
      case ArrayDatatype(dt, size) => dt.resolvePostscript + s"[$size]"
      case _                       => ""
    }
  }

  def resolveFlattendSize : Int = {
    this match {
      case ArrayDatatype(dt, size) => dt.resolveFlattendSize * size
      case _                       => 1
    }
  }
}

case class SpecialDatatype(typeName : String) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << typeName
  override def prettyprint_mpi = typeName
}

case object UnitDatatype extends Datatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "void"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case object BooleanDatatype extends Datatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "bool"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case object IntegerDatatype extends Datatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "int"
  override def prettyprint_mpi = "MPI_INT"
}

case object RealDatatype extends Datatype {
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
}

case object SIMD_RealDatatype extends Datatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = {
    val suffix = if (Knowledge.useDblPrecision) "d" else ""
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "__m128" << suffix
      case "AVX" | "AVX2" => out << "__m256" << suffix
      case "QPX"          => out << "vector4double" // no suffix
      case "NEON"         => out << "float32x4_t" // FIXME: only single precision until now
    }
  }

  override def prettyprint_mpi = "INVALID DATATYPE: " + this.prettyprint()
}

case object StringDatatype extends Datatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "std::string"
  override def prettyprint_mpi = "MPI::CHAR"
}

case object CharDatatype extends Datatype {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "char"
  override def prettyprint_mpi = "MPI::CHAR"
}

case class ArrayDatatype(datatype : Datatype, size : Int) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '[' << size << ']'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class VectorDatatype(var datatype : Datatype, var size : Int, var isRow : Option[Boolean]) extends Datatype {
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
  override def prettyprint(out : PpStream) : Unit = {
    if (isRow.getOrElse(true)) out << "Matrix<" << datatype << ',' << size << ",1>"
    else out << "Matrix<" << datatype << ",1" << size << '>'
  }
}

case class MatrixDatatype(datatype : Datatype, sizeM : Int, sizeN : Int) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << "Matrix<" << datatype << ',' << sizeM << ',' << sizeN << '>'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class PointerDatatype(datatype : Datatype) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '*'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class ReferenceDatatype(datatype : Datatype) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '&'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class ConstPointerDatatype(datatype : Datatype) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << "* const"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class ComplexDatatype(datatype : Datatype) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << "std::complex<" << datatype << '>'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

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
