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

package exastencils.base.l1

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._

trait L1_Index extends L1_Expression {
  override def progress : L2_Index

  // to be implemented by inheriting from L1_ArrayBasedIndex
  def length() : Int
}

trait L1_ArrayBasedIndex[T] extends Iterable[T] {
  var indices : Array[T]

  override def iterator() : scala.collection.Iterator[T] = indices.iterator

  def apply(i : Int) = indices.apply(i)
  def update(i : Int, x : T) = indices.update(i, x)
  def length = indices.length
}

/// L1_ConstIndex

object L1_ConstIndex {
  def apply(indices : Int*) = new L1_ConstIndex(indices.toArray)
  def apply(left : L1_ConstIndex, right : L1_ConstIndex, f : (Int, Int) => Int) =
    new L1_ConstIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => f(left(i), right(i))).toArray)
}

case class L1_ConstIndex(override var indices : Array[Int]) extends L1_Index with L1_ArrayBasedIndex[Int] {
  override def prettyprint(out : PpStream) = out << '[' << indices.mkString(", ") << ']'
  override def progress = ProgressLocation(L2_ConstIndex(indices))

  def +(that : L1_ConstIndex) = L1_ConstIndex(this, that, _ + _)
  def -(that : L1_ConstIndex) = L1_ConstIndex(this, that, _ - _)

  override def equals(other : Any) : Boolean = {
    if (this eq other.asInstanceOf[AnyRef])
      return true
    other match {
      case L1_ConstIndex(oIndices) => java.util.Arrays.equals(this.indices, oIndices)
      case _                       => false
    }
  }
}
