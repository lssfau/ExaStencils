package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

trait Datatype extends ProgressableToIr with PrettyPrintable {
  override def progressToIr : ir.Datatype
  def dimensionality : Int
  def getSizeArray : Array[Int]
}

// TODO: convert to objects
// TODO: merge with ir datatypes?

case class UnitDatatype() extends Datatype {
  override def prettyprint(out : PpStream) = { out << "Unit" }
  override def progressToIr : ir.Datatype = ir.UnitDatatype
  override def dimensionality : Int = { Logger.warn("Request for dimensionality of l4.UnitDatatype occured"); 0 }
  override def getSizeArray : Array[Int] = { Logger.warn("Request for sizeArray of l4.UnitDatatype occured"); Array() }
}

case class IntegerDatatype() extends Datatype {
  override def prettyprint(out : PpStream) = { out << "Integer" }
  override def progressToIr : ir.Datatype = ir.IntegerDatatype
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
}

case class RealDatatype() extends Datatype {
  override def prettyprint(out : PpStream) = { out << "Real" }
  override def progressToIr : ir.Datatype = ir.RealDatatype
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
}

case class StringDatatype() extends Datatype {
  override def prettyprint(out : PpStream) = { out << "String" }
  override def progressToIr : ir.Datatype = ir.StringDatatype
  override def dimensionality : Int = { Logger.warn("Request for dimensionality of l4.StringDatatype occured"); 0 }
  override def getSizeArray : Array[Int] = { Logger.warn("Request for sizeArray of l4.StringDatatype occured"); Array() }
}

case class CharDatatype() extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << "char"
  override def progressToIr : ir.Datatype = ir.CharDatatype
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
}

case class BooleanDatatype() extends Datatype {
  override def prettyprint(out : PpStream) = { out << "Boolean" }
  override def progressToIr : ir.Datatype = ir.BooleanDatatype
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
}

case class ArrayDatatype(var datatype : Datatype, var numElements : Int) extends Datatype {
  override def prettyprint(out : PpStream) = { out << "Array[" << datatype << "][" << numElements << ']' }
  override def progressToIr : ir.Datatype = new ir.ArrayDatatype(datatype.progressToIr, numElements)
  override def dimensionality : Int = 1 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array(numElements) ++ datatype.getSizeArray
}

case class ComplexDatatype(var datatype : Datatype) extends Datatype {
  override def prettyprint(out : PpStream) = { out << "Complex[" << datatype << ']' }
  override def progressToIr : ir.Datatype = new ir.ComplexDatatype(datatype.progressToIr)
  override def dimensionality : Int = 0 // TODO: likely depends on later implementation (std::complex vs Real[2])
  override def getSizeArray : Array[Int] = Array(1) // TODO: likely depends on later implementation (std::complex vs Real[2])
}

// TODO: use after successful integration:
//case class VectorDatatype(var datatype : Datatype, var length : Int, var isRow : Option[Boolean]) extends Datatype {
//  override def prettyprint(out : PpStream) = { out << "Vector[" << datatype << ',' << length << ']' }
//  override def progressToIr : ir.Datatype = new ir.VectorDatatype(datatype.progressToIr, length, isRow)
//  override def dimensionality : Int = 1 + datatype.dimensionality
//  override def getSizeArray : Array[Int] = Array(length) ++ datatype.getSizeArray
//}
case class VectorDatatype(var datatype : Datatype, var length : Int, var isRow : Option[Boolean]) extends Datatype {
  override def prettyprint(out : PpStream) = { out << "Vector[" << datatype << ',' << length << ']' }
  override def progressToIr : ir.Datatype = new ir.VectorDatatype(datatype.progressToIr, length, isRow)
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
}

// TODO: use after successful integration:
//case class MatrixDatatype(var datatype : Datatype, var rows : Int, var columns : Int) extends Datatype {
//  override def prettyprint(out : PpStream) = { out << "Matrix[" << datatype << ',' << rows << ',' << columns << ']' }
//  override def progressToIr : ir.Datatype = new ir.MatrixDatatype(datatype.progressToIr, rows, columns)
//  override def dimensionality : Int = 2 + datatype.dimensionality
//  override def getSizeArray : Array[Int] = Array(rows, columns) ++ datatype.getSizeArray
//}
case class MatrixDatatype(var datatype : Datatype, var rows : Int, var columns : Int) extends Datatype {
  override def prettyprint(out : PpStream) = { out << "Matrix[" << datatype << ',' << rows << ',' << columns << ']' }
  override def progressToIr : ir.Datatype = new ir.MatrixDatatype(datatype.progressToIr, rows, columns)
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
}
