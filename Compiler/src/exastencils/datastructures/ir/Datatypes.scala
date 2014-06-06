package exastencils.datastructures.ir

import exastencils.datastructures._
import exastencils.datastructures.ir._

trait Datatype extends Node with CppPrettyPrintable

object ResolveDatatypePre {
  def apply(dataType : Datatype) : String = {
    dataType match {
      case ArrayDatatype(dt, size) => ResolveDatatypePre(dt)
      case _                       => dataType.cpp
    }
  }
}

object ResolveDatatypePost {
  def apply(dataType : Datatype) : String = {
    dataType match {
      case ArrayDatatype(dt, size) => ResolveDatatypePost(dt) + s"[$size]"
      case _                       => ""
    }
  }
}

case class SpecialDatatype(typeName : String) extends Datatype {
  override def cpp = typeName
}

case class UnitDatatype() extends Datatype {
  override def cpp = "void"
}

case class BooleanDatatype() extends Datatype {
  override def cpp = "bool"
}

case class IntegerDatatype() extends Datatype {
  override def cpp = "int"
}

case class RealDatatype() extends Datatype {
  override def cpp = "double"
}

case class StringDatatype() extends Datatype {
  override def cpp = "std::string"
}

case class ArrayDatatype(datatype : Datatype, size : Int) extends Datatype {
  override def cpp = s"${datatype.cpp}[$size]"
}

case class PointerDatatype(datatype : Datatype) extends Datatype {
  override def cpp = s"${datatype.cpp}*"
}

case class ComplexDatatype(datatype : Datatype) extends Datatype {
  override def cpp = s"std::complex<${datatype.cpp}>"
}
