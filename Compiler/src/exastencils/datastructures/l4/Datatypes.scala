package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._

trait Datatype extends ProgressableToIr {
  override def progressToIr : ir.Datatype
}

case class IntegerDatatype() extends Datatype {
  def progressToIr : ir.Datatype = new ir.IntegerDatatype
}

case class RealDatatype() extends Datatype {
  def progressToIr : ir.Datatype = new ir.RealDatatype
}

case class StringDatatype() extends Datatype {
  def progressToIr : ir.Datatype = new ir.StringDatatype
}

case class UnitDatatype() extends Datatype {
  def progressToIr : ir.Datatype = new ir.UnitDatatype
}

case class ArrayDatatype(var datatype : Datatype) extends Datatype {
  def progressToIr : ir.Datatype = new ir.ArrayDatatype(datatype.progressToIr)
}

case class ComplexDatatype(var datatype : Datatype) extends Datatype {
  def progressToIr : ir.Datatype = new ir.ComplexDatatype(datatype.progressToIr)
}
