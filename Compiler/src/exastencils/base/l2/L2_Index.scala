package exastencils.base.l2

import exastencils.base.l3._
import exastencils.core._
import exastencils.prettyprinting._

/// L2_Index

trait L2_Index extends L2_Expression {
  override def progress : L3_Index
  def toExpressionIndex : L2_ExpressionIndex
  def length : Int
}

/// L2_ArrayBasedIndex

trait L2_ArrayBasedIndex[T] extends Iterable[T] {
  var indices : Array[T]

  override def iterator() : scala.collection.Iterator[T] = indices.iterator

  def apply(i : Int) = indices.apply(i)
  def update(i : Int, x : T) = indices.update(i, x)
  def length = indices.length
}

/// L2_ExpressionIndex

object L2_ExpressionIndex {
  def apply(indices : L2_Expression*) = new L2_ExpressionIndex(indices.toArray)
  def apply(left : L2_ExpressionIndex, right : L2_ExpressionIndex, f : (L2_Expression, L2_Expression) => L2_Expression) =
    new L2_ExpressionIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => Duplicate(f(left(i), right(i)))).toArray)
}

case class L2_ExpressionIndex(override var indices : Array[L2_Expression]) extends L2_Index with L2_ArrayBasedIndex[L2_Expression] {
  override def prettyprint(out : PpStream) = out << '[' <<< (this, ", ") << ']'
  override def progress = L3_ExpressionIndex(indices.map(_.progress))

  def +(that : L2_Index) = {
    that match {
      case that : L2_ExpressionIndex => L2_ExpressionIndex(this, that, _ + _)
      case that : L2_ConstIndex      => L2_ExpressionIndex(this, that.toExpressionIndex, _ + _)
    }
  }
  def -(that : L2_Index) = {
    that match {
      case that : L2_ExpressionIndex => L2_ExpressionIndex(this, that, _ - _)
      case that : L2_ConstIndex      => L2_ExpressionIndex(this, that.toExpressionIndex, _ - _)
    }
  }

  override def equals(other : Any) : Boolean = {
    if (this eq other.asInstanceOf[AnyRef])
      return true
    other match {
      case L2_ExpressionIndex(oIndices) => java.util.Arrays.equals(this.indices.asInstanceOf[Array[Object]], oIndices.asInstanceOf[Array[Object]])
      case _                            => false
    }
  }

  override def hashCode() : Int = {
    java.util.Arrays.hashCode(indices.asInstanceOf[Array[Object]]) * 31 + 22 // random modification to ensure the hashcode of this element differs from the hashcode of the array itself
  }

  override def toExpressionIndex = this
}

/// L2_ConstIndex

object L2_ConstIndex {
  def apply(indices : Int*) = new L2_ConstIndex(indices.toArray)
  def apply(left : L2_ConstIndex, right : L2_ConstIndex, f : (Int, Int) => Int) =
    new L2_ConstIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => f(left(i), right(i))).toArray)
}

case class L2_ConstIndex(override var indices : Array[Int]) extends L2_Index with L2_ArrayBasedIndex[Int] {
  override def prettyprint(out : PpStream) = out << '[' << indices.mkString(", ") << ']'
  override def progress = L3_ConstIndex(indices)

  def +(that : L2_ConstIndex) = L2_ConstIndex(this, that, _ + _)
  def -(that : L2_ConstIndex) = L2_ConstIndex(this, that, _ - _)

  def +(that : L2_ExpressionIndex) = L2_ExpressionIndex(this.toExpressionIndex, that, _ + _)
  def -(that : L2_ExpressionIndex) = L2_ExpressionIndex(this.toExpressionIndex, that, _ - _)

  override def toExpressionIndex = L2_ExpressionIndex(indices.map(L2_IntegerConstant(_) : L2_Expression))
}
