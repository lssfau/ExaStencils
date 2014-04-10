package exastencils.datastructures.ir

import exastencils.datastructures._
import exastencils.datastructures.ir._

trait Datatype extends Node with CppPrettyPrintable

case class SpecialDatatype(typeName : String) extends Datatype {
  override def cpp = typeName
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

case class UnitDatatype() extends Datatype {
  override def cpp = "void"
}

case class ArrayDatatype(datatype : Datatype) extends Datatype {
  override def cpp = "array of " + datatype.cpp
}

case class ComplexDatatype(datatype : Datatype) extends Datatype {
  override def cpp = "complex of " + datatype.cpp
}
