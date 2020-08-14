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

package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.core._
import exastencils.logger.Logger
import exastencils.optimization.l4.L4_SimplifyExpression
import exastencils.prettyprinting._

trait L4_Index extends L4_Expression {
  override def progress : IR_Index

  // to be implemented by inheriting from L4_ArrayBasedIndex
  def length() : Int

  // conversion to expression index
  def toExpressionIndex : L4_ExpressionIndex

  // index arithmetic
  def +(that : L4_Index) : L4_Index
  def -(that : L4_Index) : L4_Index
}

trait L4_ArrayBasedIndex[T] extends Iterable[T] {
  var indices : Array[T]

  override def iterator() : scala.collection.Iterator[T] = indices.iterator

  def apply(i : Int) = indices.apply(i)
  def update(i : Int, x : T) = indices.update(i, x)
  def length = indices.length
}

/// L4_ExpressionIndex

object L4_ExpressionIndex {
  def apply(indices : L4_Expression*) = new L4_ExpressionIndex(indices.toArray)
  def apply(left : L4_ExpressionIndex, right : L4_ExpressionIndex, f : (L4_Expression, L4_Expression) => L4_Expression) =
    new L4_ExpressionIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => Duplicate(f(left(i), right(i)))).toArray)
  def apply(indices : Array[Int]) = new L4_ExpressionIndex(indices.map(L4_IntegerConstant(_) : L4_Expression))
}

case class L4_ExpressionIndex(override var indices : Array[L4_Expression]) extends L4_Index with L4_ArrayBasedIndex[L4_Expression] {
  override def prettyprint(out : PpStream) = out << '[' <<< (this, ", ") << ']'
  override def progress = ProgressLocation(IR_ExpressionIndex(indices.map(_.progress)))

  override def +(that : L4_Index) : L4_ExpressionIndex = L4_ExpressionIndex(this, that.toExpressionIndex, _ + _)
  override def -(that : L4_Index) : L4_ExpressionIndex = L4_ExpressionIndex(this, that.toExpressionIndex, _ - _)

  override def toExpressionIndex = this

  def toConstIndex = {
    L4_ConstIndex(indices.map(L4_SimplifyExpression.simplifyIntegralExpr(_) match {
      case L4_IntegerConstant(value) => value.toInt
      case other                     => Logger.error(s"Unsupported value in (constant) index: $other")
    }))
  }

  override def equals(other : Any) : Boolean = {
    if (this eq other.asInstanceOf[AnyRef])
      return true
    other match {
      case L4_ExpressionIndex(oIndices) => java.util.Arrays.equals(this.indices.asInstanceOf[Array[Object]], oIndices.asInstanceOf[Array[Object]])
      case _                            => false
    }
  }

  override def hashCode() : Int = {
    java.util.Arrays.hashCode(indices.asInstanceOf[Array[Object]]) * 31 + 42 // random modification to ensure the hashcode of this element differs from the hashcode of the array itself
  }
}

/// L4_ConstIndex

object L4_ConstIndex {
  def apply(indices : Int*) = new L4_ConstIndex(indices.toArray)
  def apply(left : L4_ConstIndex, right : L4_ConstIndex, f : (Int, Int) => Int) =
    new L4_ConstIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => f(left(i), right(i))).toArray)
}

case class L4_ConstIndex(override var indices : Array[Int]) extends L4_Index with L4_ArrayBasedIndex[Int] {
  override def prettyprint(out : PpStream) = out << '[' << indices.mkString(", ") << ']'
  override def progress = ProgressLocation(IR_ConstIndex(indices))

  def +(that : L4_ConstIndex) = L4_ConstIndex(this, that, _ + _)
  override def +(that : L4_Index) = L4_ExpressionIndex(this.toExpressionIndex, that.toExpressionIndex, _ + _)

  def -(that : L4_ConstIndex) = L4_ConstIndex(this, that, _ - _)
  override def -(that : L4_Index) = L4_ExpressionIndex(this.toExpressionIndex, that.toExpressionIndex, _ - _)

  override def toExpressionIndex = L4_ExpressionIndex(indices.map(L4_IntegerConstant(_) : L4_Expression))

  override def equals(other : Any) : Boolean = {
    if (this eq other.asInstanceOf[AnyRef])
      return true
    other match {
      case L4_ConstIndex(oIndices) => java.util.Arrays.equals(this.indices, oIndices)
      case _                       => false
    }
  }
}

/// L4_RangeIndex

case class L4_Range(var begin : Option[L4_Expression], var end : Option[L4_Expression]) {
  if (begin.isEmpty && end.isEmpty) {
    Logger.warn("Empty L4_Range")
  }
  def progress() : IR_Range = IR_Range(
    if(begin.isDefined) Some(begin.get.progress) else None,
    if(end.isDefined) Some(end.get.progress) else None
  )
}

object L4_RangeIndex {
  def apply(indices : L4_Range*) = new L4_RangeIndex(indices.toArray)
}

case class L4_RangeIndex(override var indices : Array[L4_Range]) extends L4_Index with L4_ArrayBasedIndex[L4_Range] {
  override def prettyprint(out : PpStream) = out << '[' << indices.mkString(", ") << ']'
  override def progress = IR_RangeIndex(indices.map(i => i.progress()))

  override def toExpressionIndex = ??? // FIXME (if a sensible conversion exists at all)
  override def +(that : L4_Index) : L4_Index = ???
  override def -(that : L4_Index) : L4_Index = ???
}
