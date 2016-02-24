package exastencils.datastructures.ir

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.iv.VecShiftIndex
import exastencils.knowledge._
import exastencils.prettyprinting._
import exastencils.strategies._

case class MultiIndex(var indices : Array[Expression]) extends Expression with Iterable[Expression] {
  def this(indices : Expression*) = this(indices.toArray)
  def this(indices : Array[Int]) = this(indices.map(IntegerConstant(_) : Expression)) // legacy support
  def this(left : MultiIndex, right : MultiIndex, f : (Expression, Expression) => Expression) =
    this((0 until math.min(left.indices.length, right.indices.length)).map(i => Duplicate(f(left(i), right(i)))).toArray)

  // FIXME: add variable accesses to begin with...
  for (i <- 0 until length) {
    update(i, indices(i) match {
      case StringLiteral(s) => VariableAccess(s, Some(IntegerDatatype))
      case _                => indices(i)
    })
  }

  override def prettyprint(out : PpStream) : Unit = {
    out << '[' <<< (this, ", ") << ']'
  }

  def +(that : MultiIndex) : MultiIndex = new MultiIndex(this, that, _ + _)
  def -(that : MultiIndex) : MultiIndex = new MultiIndex(this, that, _ - _)

  // expose array functions
  override def iterator() : scala.collection.Iterator[Expression] = indices.iterator

  def apply(i : Int) = indices.apply(i)
  def update(i : Int, x : Expression) = indices.update(i, x)
  def length = indices.length
}

case class ConstIndex(var indices : Array[Int]) extends Expression with Iterable[Int] {
  def this(indices : Int*) = this(indices.toArray)
  def this(left : ConstIndex, right : ConstIndex, f : (Int, Int) => Int) =
    this((0 until math.min(left.indices.length, right.indices.length)).map(i => Duplicate(f(left(i), right(i)))).toArray)

  override def prettyprint(out : PpStream) : Unit = {
    out << '[' << indices.mkString(", ") << ']'
  }

  def +(that : ConstIndex) : ConstIndex = new ConstIndex(this, that, _ + _)
  def -(that : ConstIndex) : ConstIndex = new ConstIndex(this, that, _ - _)

  // expose array functions
  override def iterator() : scala.collection.Iterator[Int] = indices.iterator

  def apply(i : Int) = indices.apply(i)
  def update(i : Int, x : Int) = indices.update(i, x)
  def length = indices.length
}
