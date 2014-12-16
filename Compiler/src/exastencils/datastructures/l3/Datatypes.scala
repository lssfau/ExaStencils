package exastencils.datastructures.l3

import exastencils.datastructures._
import exastencils.datastructures.l3._

trait Datatype

case class IntegerDatatype() extends Datatype/* {
  def progressToIr : ir.Datatype = new ir.IntegerDatatype
}*/

case class RealDatatype() extends Datatype/* {
  def progressToIr : ir.Datatype = new ir.RealDatatype
}*/

case class StringDatatype() extends Datatype/* {
  def progressToIr : ir.Datatype = new ir.StringDatatype
}*/

case class UnitDatatype() extends Datatype/* {
  def progressToIr : ir.Datatype = new ir.UnitDatatype
}*/

case class ArrayDatatype(var datatype : Datatype, var numElements : Int) extends Datatype/* {
  def progressToIr : ir.Datatype = new ir.ArrayDatatype(datatype.progressToIr, numElements)
}*/

case class ComplexDatatype(var datatype : Datatype) extends Datatype/* {
  def progressToIr : ir.Datatype = new ir.ComplexDatatype(datatype.progressToIr)
}*/

case class FieldDatatype() extends Datatype
case class StencilDatatype() extends Datatype

