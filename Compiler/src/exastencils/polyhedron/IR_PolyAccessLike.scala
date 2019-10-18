//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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
