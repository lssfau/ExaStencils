package exastencils.datastructures.ir

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge.Knowledge

trait Datatype extends Node with CppPrettyPrintable {
  def cpp_mpi : String

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
  override def cpp = typeName
  override def cpp_mpi = typeName
}

case class UnitDatatype() extends Datatype {
  override def cpp = "void"
  override def cpp_mpi = s"INVALID DATATYPE: $cpp"
}

case class BooleanDatatype() extends Datatype {
  override def cpp = "bool"
  override def cpp_mpi = s"INVALID DATATYPE: $cpp"
}

case class IntegerDatatype() extends Datatype {
  override def cpp = "int"
  override def cpp_mpi = "MPI_INT"
}

case class RealDatatype() extends Datatype {
  override def cpp = "double"
  override def cpp_mpi = "MPI_DOUBLE"
}

case class SIMD_RealDatatype() extends Datatype {
  override def cpp = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => "__m128d"
      case "AVX" | "AVX2" => "__m256d"
    }
  }

  override def cpp_mpi = "INVALID DATATYPE: " + cpp
}

case class StringDatatype() extends Datatype {
  override def cpp = "std::string"
  override def cpp_mpi = "MPI::CHAR"
}

case class ArrayDatatype(datatype : Datatype, size : Int) extends Datatype {
  override def cpp = s"${datatype.cpp}[$size]"
  override def cpp_mpi = s"INVALID DATATYPE: $cpp"
}

case class PointerDatatype(datatype : Datatype) extends Datatype {
  override def cpp = s"${datatype.cpp}*"
  override def cpp_mpi = s"INVALID DATATYPE: $cpp"
}

case class ComplexDatatype(datatype : Datatype) extends Datatype {
  override def cpp = s"std::complex<${datatype.cpp}>"
  override def cpp_mpi = s"INVALID DATATYPE: $cpp"
}
