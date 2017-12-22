package exastencils.polyhedron

import scala.collection.mutable.StringBuilder

import exastencils.base.ir.IR_Expression

/**
  * Is used for IR nodes that should be seen as an array access when extracting a polyhedral representation.
  */
trait IR_PolyArrayAccessLike {

  /** @return the index expression for the access. */
  def index : IR_Expression

  /** @return a unique (C/C++) identifier for the base of this access (without taking the index into account). */
  def uniqueID : String

  // should be inherited from scala.Product (each case class is a subclass of scala.Product)
  def productIterator : Iterator[Any]

  def replaceSpecial(str : StringBuilder) : StringBuilder = {
    var i : Int = 0
    while (i < str.length) {
      str(i) match {
        case '.' | '[' | ']' | '(' | ')' | '-' | '>' => str(i) = '_'
        case _                                       =>
      }
      i += 1
    }
    str
  }
}

/**
  * Is used for IR nodes that should be seen as a scalar access when extracting a polyhedral representation.
  */
trait IR_PolyScalarAccessLike {

  /** @return a unique (C/C++) identifier for the base of this access (without taking the index into account). */
  def uniqueID : String

  // should be inherited from scala.Product (each case class is a subclass of scala.Product)
  def productIterator : Iterator[Any]
}
