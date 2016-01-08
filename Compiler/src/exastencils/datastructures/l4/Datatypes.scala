package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.prettyprinting._

trait Datatype extends ProgressableToIr with PrettyPrintable {
  override def progressToIr : ir.Datatype
}

case class IntegerDatatype() extends Datatype {
  def prettyprint(out : PpStream) = { out << "Integer" }
  def progressToIr : ir.Datatype = ir.IntegerDatatype
}

case class RealDatatype() extends Datatype {
  def prettyprint(out : PpStream) = { out << "Real" }
  def progressToIr : ir.Datatype = ir.RealDatatype
}

case class StringDatatype() extends Datatype {
  def prettyprint(out : PpStream) = { out << "String" }
  def progressToIr : ir.Datatype = ir.StringDatatype
}

case class CharDatatype() extends Datatype {
  def prettyprint(out : PpStream) : Unit = out << "char"
  def progressToIr : ir.Datatype = ir.CharDatatype
}

case class UnitDatatype() extends Datatype {
  def prettyprint(out : PpStream) = { out << "Unit" }
  def progressToIr : ir.Datatype = ir.UnitDatatype
}

case class ComplexDatatype(var datatype : Datatype) extends Datatype {
  def prettyprint(out : PpStream) = { out << "Complex[" << datatype << ']' }
  def progressToIr : ir.Datatype = new ir.ComplexDatatype(datatype.progressToIr)
}

case class VectorDatatype(var datatype : Datatype, var length : Int, var isRow : Option[Boolean]) extends Datatype {
  def prettyprint(out : PpStream) = { out << "Vector[" << datatype << ',' << length << ']' }
  def progressToIr : ir.Datatype = new ir.VectorDatatype(datatype.progressToIr, length, isRow)
}

case class MatrixDatatype(var datatype : Datatype, var rows : Int, var columns : Int) extends Datatype {
  def prettyprint(out : PpStream) = { out << "Matrix[" << datatype << ',' << rows << ',' << columns << ']' }
  def progressToIr : ir.Datatype = new ir.MatrixDatatype(datatype.progressToIr, rows, columns)
}

case class BooleanDatatype() extends Datatype {
  def prettyprint(out : PpStream) = { out << "Boolean" }
  def progressToIr : ir.Datatype = ir.BooleanDatatype
}
