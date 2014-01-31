package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._

trait Datatype

case class IntegerDatatype() extends Datatype

case class RealDatatype() extends Datatype

case class StringDatatype() extends Datatype

case class UnitDatatype() extends Datatype

case class ArrayDatatype(datatype : Datatype) extends Datatype

case class ComplexDatatype(datatype : Datatype) extends Datatype
