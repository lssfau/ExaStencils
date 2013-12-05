package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._

trait Datatype extends Duplicable

case class IntegerDatatype() extends Datatype {
  override def duplicate = copy.asInstanceOf[this.type]
}

case class RealDatatype() extends Datatype {
  override def duplicate = copy.asInstanceOf[this.type]
}

case class StringDatatype() extends Datatype {
  override def duplicate = copy.asInstanceOf[this.type]
}

case class UnitDatatype() extends Datatype {
  override def duplicate = copy.asInstanceOf[this.type]
}

case class ArrayDatatype(datatype : Datatype) extends Datatype {
  override def duplicate = copy(datatype = Duplicate(datatype)).asInstanceOf[this.type]
}

case class ComplexDatatype(datatype : Datatype) extends Datatype {
  override def duplicate = copy(datatype = Duplicate(datatype)).asInstanceOf[this.type]
}
