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

case class ArrayDatatype(var datatype : Datatype, var numElements : Int) extends Datatype {
  def prettyprint(out : PpStream) = { out << "Array[" << datatype << "][" << numElements << ']' }
  def progressToIr : ir.Datatype = new ir.ArrayDatatype(datatype.progressToIr, numElements)
}

case class ComplexDatatype(var datatype : Datatype) extends Datatype {
  def prettyprint(out : PpStream) = { out << "Complex[" << datatype << ']' }
  def progressToIr : ir.Datatype = new ir.ComplexDatatype(datatype.progressToIr)
}

case class VectorDatatype(var datatype : Datatype, var size : Int) extends Datatype {
  def prettyprint(out : PpStream) = { out << "Vector[" << datatype << ']' << '[' << size << ']' }
  def progressToIr : ir.Datatype = ???
}

case class MatrixDatatype(var datatype : Datatype, var sizeM : Int, var sizeN : Int) extends Datatype {
  def prettyprint(out : PpStream) = { out << "Matrix[" << datatype << ']' << '[' << sizeM << ']' << '[' << sizeN << ']' }
  def progressToIr : ir.Datatype = ???
}
