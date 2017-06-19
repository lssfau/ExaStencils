package exastencils.base.ir

import exastencils.core.Duplicate
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.prettyprinting.PpStream

trait IR_Index extends IR_Expression {
  // to be implemented by inheriting from IR_ArrayBasedIndex
  def length() : Int

  // conversion to expression index
  def toExpressionIndex : IR_ExpressionIndex

  // index arithmetic
  def +(that : IR_Index) : IR_Index
  def -(that : IR_Index) : IR_Index
}

trait IR_ArrayBasedIndex[T] extends Iterable[T] {
  var indices : Array[T]

  override def iterator() : scala.collection.Iterator[T] = indices.iterator

  def apply(i : Int) = indices.apply(i)
  def update(i : Int, x : T) = indices.update(i, x)
  def length = indices.length
}

/// IR_ExpressionIndex

object IR_ExpressionIndex {
  def apply(indices : IR_Expression*) = new IR_ExpressionIndex(indices.toArray)
  def apply(left : IR_ExpressionIndex, right : IR_ExpressionIndex, f : (IR_Expression, IR_Expression) => IR_Expression) =
    new IR_ExpressionIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => Duplicate(f(left(i), right(i)))).toArray)

  // legacy support
  def apply(indices : Array[Int]) = new IR_ExpressionIndex(indices.map(IR_IntegerConstant(_) : IR_Expression))
  def apply(indices : Array[Long]) = new IR_ExpressionIndex(indices.map(IR_IntegerConstant(_) : IR_Expression))
}

case class IR_ExpressionIndex(var indices : Array[IR_Expression]) extends IR_Index with IR_ArrayBasedIndex[IR_Expression] {
  // FIXME: add variable accesses to begin with...
  for (i <- 0 until length) {
    update(i, indices(i) match {
      case IR_StringLiteral(s) => IR_VariableAccess(s, IR_IntegerDatatype)
      case _                   => indices(i)
    })
  }

  // FIXME
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) = out << '[' <<< (this, ", ") << ']'

  override def +(that : IR_Index) : IR_ExpressionIndex = {
    that match {
      case that : IR_ExpressionIndex => IR_ExpressionIndex(this, that, _ + _)
      case that : IR_ConstIndex      => IR_ExpressionIndex(this, that.toExpressionIndex, _ + _)
    }
  }
  override def -(that : IR_Index) : IR_ExpressionIndex = {
    that match {
      case that : IR_ExpressionIndex => IR_ExpressionIndex(this, that, _ - _)
      case that : IR_ConstIndex      => IR_ExpressionIndex(this, that.toExpressionIndex, _ - _)
    }
  }

  override def toExpressionIndex = this

  def toConstIndex = {
    IR_ConstIndex(indices.map(IR_SimplifyExpression.simplifyIntegralExpr(_) match {
      case IR_IntegerConstant(value) => value.toInt
      case other                     => Logger.error(s"Unsupported value in (constant) index: $other")
    }))
  }

  override def equals(other : Any) : Boolean = {
    if (this eq other.asInstanceOf[AnyRef])
      return true
    other match {
      case IR_ExpressionIndex(oIndices) => java.util.Arrays.equals(this.indices.asInstanceOf[Array[Object]], oIndices.asInstanceOf[Array[Object]])
      case _                            => false
    }
  }

  override def hashCode() : Int = {
    java.util.Arrays.hashCode(indices.asInstanceOf[Array[Object]]) * 31 + 42 // random modification to ensure the hashcode of this element differs from the hashcode of the array itself
  }
}

/// IR_ConstIndex

object IR_ConstIndex {
  def apply(indices : Int*) = new IR_ConstIndex(indices.toArray)
  def apply(left : IR_ConstIndex, right : IR_ConstIndex, f : (Int, Int) => Int) =
    new IR_ConstIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => f(left(i), right(i))).toArray)
}

case class IR_ConstIndex(override var indices : Array[Int]) extends IR_Index with IR_ArrayBasedIndex[Int] {
  override def datatype = /*FIXME*/ IR_UnitDatatype
  override def prettyprint(out : PpStream) = out << '[' << indices.mkString(", ") << ']'

  def +(that : IR_ConstIndex) = IR_ConstIndex(this, that, _ + _)
  override def +(that : IR_Index) = IR_ExpressionIndex(this.toExpressionIndex, that.toExpressionIndex, _ + _)

  def -(that : IR_ConstIndex) = IR_ConstIndex(this, that, _ - _)
  override def -(that : IR_Index) = IR_ExpressionIndex(this.toExpressionIndex, that.toExpressionIndex, _ - _)

  override def toExpressionIndex = IR_ExpressionIndex(indices.map(IR_IntegerConstant(_) : IR_Expression))

  override def equals(other : Any) : Boolean = {
    if (this eq other.asInstanceOf[AnyRef])
      return true
    other match {
      case IR_ConstIndex(oIndices) => java.util.Arrays.equals(this.indices, oIndices)
      case _                       => false
    }
  }
}

/// IR_HighDimIndex

/*
Idea for later: High Dimensional Index: [start, stride, stop], e.g., to select subsets of matrices
TODO: think about how to implement other selection, e.g. "take main diagonal" or "right upper matrix"

object IR_HighDimIndex {
  def apply(indices : (IR_Expression, IR_Expression, IR_Expression)*) = new IR_HighDimIndex(indices.toArray)
  def apply(left : IR_HighDimIndex, right : IR_HighDimIndex, f : ((IR_Expression, IR_Expression, IR_Expression), (IR_Expression, IR_Expression, IR_Expression)) => (IR_Expression, IR_Expression, IR_Expression)) =
    new IR_HighDimIndex((0 until math.min(left.indices.length, right.indices.length)).map(i => Duplicate(f(left(i), right(i)))).toArray)
}

case class IR_HighDimIndex(var indices : Array[(IR_Expression, IR_Expression, IR_Expression)]) extends IR_Index with IR_ArrayBasedIndex[(IR_Expression, IR_Expression, IR_Expression)] {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) = {
    out << "INVALID: IR_HighDimIndex("
    indices.foreach(x => {x._1.prettyprint(out)
      out << " : "
      x._2.prettyprint(out)
      out << " : "
      x._3.prettyprint(out)})
    out << ")"
  }
  override def +(that : IR_Index) = {

  }

  override def -(that : IR_Index) = {

  }
}
*/
