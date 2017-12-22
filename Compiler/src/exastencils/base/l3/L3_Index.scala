package exastencils.base.l3

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.core._
import exastencils.logger.Logger
import exastencils.optimization.l3.L3_SimplifyExpression
import exastencils.prettyprinting._

trait L3_Index extends L3_Expression {
  override def progress : L4_Index

  // to be implemented by inheriting from L3_ArrayBasedIndex
  def length() : Int

  // conversion to expression index
  def toExpressionIndex : L3_ExpressionIndex

  // index arithmetic
  def +(that : L3_Index) : L3_Index
  def -(that : L3_Index) : L3_Index
}

trait L3_ArrayBasedIndex[T] extends Iterable[T] {
  var indices : Array[T]

  override def iterator() : scala.collection.Iterator[T] = indices.iterator

  def apply(i : Int) = indices.apply(i)
  def update(i : Int, x : T) = indices.update(i, x)
  def length = indices.length
}

/// L3_ExpressionIndex

object L3_ExpressionIndex {
  def apply(indices : L3_Expression*) = new L3_ExpressionIndex(indices.toArray)
  def apply(left : L3_ExpressionIndex, right : L3_ExpressionIndex, f : (L3_Expression, L3_Expression) => L3_Expression) =
    new L3_ExpressionIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => Duplicate(f(left(i), right(i)))).toArray)
}

case class L3_ExpressionIndex(override var indices : Array[L3_Expression]) extends L3_Index with L3_ArrayBasedIndex[L3_Expression] {
  override def prettyprint(out : PpStream) = out << '[' <<< (this, ", ") << ']'
  override def progress = ProgressLocation(L4_ExpressionIndex(indices.map(_.progress)))

  override def +(that : L3_Index) : L3_ExpressionIndex = L3_ExpressionIndex(this, that.toExpressionIndex, _ + _)
  override def -(that : L3_Index) : L3_ExpressionIndex = L3_ExpressionIndex(this, that.toExpressionIndex, _ - _)

  override def toExpressionIndex = this

  def toConstIndex = {
    L3_ConstIndex(indices.map(L3_SimplifyExpression.simplifyIntegralExpr(_) match {
      case L3_IntegerConstant(value) => value.toInt
      case other                     => Logger.error(s"Unsupported value in (constant) index: $other")
    }))
  }

  override def equals(other : Any) : Boolean = {
    if (this eq other.asInstanceOf[AnyRef])
      return true
    other match {
      case L3_ExpressionIndex(oIndices) => java.util.Arrays.equals(this.indices.asInstanceOf[Array[Object]], oIndices.asInstanceOf[Array[Object]])
      case _                            => false
    }
  }

  override def hashCode() : Int = {
    java.util.Arrays.hashCode(indices.asInstanceOf[Array[Object]]) * 31 + 42 // random modification to ensure the hashcode of this element differs from the hashcode of the array itself
  }
}

/// L3_ConstIndex

object L3_ConstIndex {
  def apply(indices : Int*) = new L3_ConstIndex(indices.toArray)
  def apply(left : L3_ConstIndex, right : L3_ConstIndex, f : (Int, Int) => Int) =
    new L3_ConstIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => f(left(i), right(i))).toArray)
}

case class L3_ConstIndex(override var indices : Array[Int]) extends L3_Index with L3_ArrayBasedIndex[Int] {
  override def prettyprint(out : PpStream) = out << '[' << indices.mkString(", ") << ']'
  override def progress = ProgressLocation(L4_ConstIndex(indices))

  def +(that : L3_ConstIndex) = L3_ConstIndex(this, that, _ + _)
  override def +(that : L3_Index) = L3_ExpressionIndex(this.toExpressionIndex, that.toExpressionIndex, _ + _)

  def -(that : L3_ConstIndex) = L3_ConstIndex(this, that, _ - _)
  override def -(that : L3_Index) = L3_ExpressionIndex(this.toExpressionIndex, that.toExpressionIndex, _ - _)

  override def toExpressionIndex = L3_ExpressionIndex(indices.map(L3_IntegerConstant(_) : L3_Expression))

  override def equals(other : Any) : Boolean = {
    if (this eq other.asInstanceOf[AnyRef])
      return true
    other match {
      case L3_ConstIndex(oIndices) => java.util.Arrays.equals(this.indices, oIndices)
      case _                       => false
    }
  }
}
