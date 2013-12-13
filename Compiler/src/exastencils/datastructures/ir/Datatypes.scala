package exastencils.datastructures.ir

import exastencils.datastructures._
import exastencils.datastructures.ir._

trait Datatype extends CppPrettyPrintable with Duplicable

case class SpecialDatatype(typeName : String) extends Datatype {
  override def cpp = typeName
  override def duplicate = this.copy().asInstanceOf[this.type] // FIXME: check for correctness
}

case class IntegerDatatype() extends Datatype {
  override def cpp = "int"
  override def duplicate = copy.asInstanceOf[this.type]
}

case class RealDatatype() extends Datatype {
  override def cpp = "double"
  override def duplicate = copy.asInstanceOf[this.type]
}

case class StringDatatype() extends Datatype {
  override def cpp = "std::string"
  override def duplicate = copy.asInstanceOf[this.type]
}

case class UnitDatatype() extends Datatype {
  override def cpp = "void"
  override def duplicate = copy.asInstanceOf[this.type]
}

case class ArrayDatatype(datatype : Datatype) extends Datatype {
  override def cpp = "array of " + datatype.cpp
  override def duplicate = copy(datatype = Duplicate(datatype)).asInstanceOf[this.type]
}

case class ComplexDatatype(datatype : Datatype) extends Datatype {
  override def cpp = "complex of " + datatype.cpp
  override def duplicate = copy(datatype = Duplicate(datatype)).asInstanceOf[this.type]
}
