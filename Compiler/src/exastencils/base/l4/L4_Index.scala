package exastencils.base.l4

import exastencils.base.ir._
import exastencils.core._
import exastencils.logger.Logger
import exastencils.prettyprinting._

trait L4_Index extends L4_Expression {
  override def progress : IR_Index
  def toExpressionIndex : L4_ExpressionIndex
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
}

case class L4_ExpressionIndex(override var indices : Array[L4_Expression]) extends L4_Index with L4_ArrayBasedIndex[L4_Expression] {
  override def prettyprint(out : PpStream) = out << '[' <<< (this, ", ") << ']'
  override def progress = IR_ExpressionIndex(indices.map(_.progress))

  def +(that : L4_Index) = {
    that match {
      case that : L4_ExpressionIndex => L4_ExpressionIndex(this, that, _ + _)
      case that : L4_ConstIndex      => L4_ExpressionIndex(this, that.toExpressionIndex, _ + _)
    }
  }
  def -(that : L4_Index) = {
    that match {
      case that : L4_ExpressionIndex => L4_ExpressionIndex(this, that, _ - _)
      case that : L4_ConstIndex      => L4_ExpressionIndex(this, that.toExpressionIndex, _ - _)
    }
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

  override def toExpressionIndex = this
}

/// L4_ConstIndex

object L4_ConstIndex {
  def apply(indices : Int*) = new L4_ConstIndex(indices.toArray)
  def apply(left : L4_ConstIndex, right : L4_ConstIndex, f : (Int, Int) => Int) =
    new L4_ConstIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => f(left(i), right(i))).toArray)
}

case class L4_ConstIndex(override var indices : Array[Int]) extends L4_Index with L4_ArrayBasedIndex[Int] {
  override def prettyprint(out : PpStream) = out << '[' << indices.mkString(", ") << ']'
  override def progress = IR_ConstIndex(indices)

  def +(that : L4_ConstIndex) = L4_ConstIndex(this, that, _ + _)
  def -(that : L4_ConstIndex) = L4_ConstIndex(this, that, _ - _)

  def +(that : L4_ExpressionIndex) = L4_ExpressionIndex(this.toExpressionIndex, that, _ + _)
  def -(that : L4_ExpressionIndex) = L4_ExpressionIndex(this.toExpressionIndex, that, _ - _)

  override def toExpressionIndex = L4_ExpressionIndex(indices.map(L4_IntegerConstant(_) : L4_Expression))
}

/// L4_RangeIndex

case class L4_Range(var begin : Option[L4_Expression], var end : Option[L4_Expression]) {
  if (begin.isEmpty == end.isEmpty == true) {
    Logger.warn("Empty L4_Range")
  }
}

object L4_RangeIndex {
  def apply(indices : L4_Range*) = new L4_RangeIndex(indices.toArray)
}

case class L4_RangeIndex(override var indices : Array[L4_Range]) extends L4_Index with L4_ArrayBasedIndex[L4_Range] {
  override def prettyprint(out : PpStream) = out << '[' << indices.mkString(", ") << ']'
  override def progress = ??? // FIXME

  override def toExpressionIndex = ??? // FIXME (if a sensible conversion exists at all)
}