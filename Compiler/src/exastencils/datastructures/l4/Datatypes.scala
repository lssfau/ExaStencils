package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._

trait Datatype extends CppPrettyPrintable
case class IntegerDatatype() extends Datatype {
  def cpp = { "int" }
}
case class RealDatatype() extends Datatype {
  def cpp = { "double" }
}
case class StringDatatype() extends Datatype {
  def cpp = { "std::string" }
}
case class UnitDatatype() extends Datatype {
  def cpp = { "void" }
}
case class ArrayDatatype(datatype : Datatype) extends Datatype {
  def cpp = { "array of " + datatype.cpp }
}
case class ComplexDatatype(datatype : Datatype) extends Datatype {
  def cpp = { "complex of " + datatype.cpp }
}
