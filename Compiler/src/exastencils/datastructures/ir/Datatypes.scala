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

case class UnitDatatype() extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << "void"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class BooleanDatatype() extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << "bool"
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class IntegerDatatype() extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << "int"
  override def prettyprint_mpi = "MPI_INT"
}

case class RealDatatype() extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << "double"
  override def prettyprint_mpi = "MPI_DOUBLE"
}

case class SIMD_RealDatatype() extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => "__m128d"
      case "AVX" | "AVX2" => "__m256d"
    }
  }

  override def prettyprint_mpi = "INVALID DATATYPE: " + this.prettyprint()
}

case class StringDatatype() extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << "std::string"
  override def prettyprint_mpi = "MPI::CHAR"
}

case class ArrayDatatype(datatype : Datatype, size : Int) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '[' << size << ']'
  override def prettyprint_mpi = s"INVALID DATATYPE: " + this.prettyprint()
}

case class PointerDatatype(datatype : Datatype) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << datatype << '*'
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
