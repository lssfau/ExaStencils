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
  //def apply(indices : Array[Int]) = new IR_ExpressionIndex(indices.map(IR_IntegerConstant(_) : IR_Expression))
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

case class IR_Range(var begin : Option[IR_Expression], var end : Option[IR_Expression]) extends IR_Expression {
  override def datatype : IR_Datatype = Logger.error("this should not be asked for")
  override def prettyprint(out : PpStream) : Unit = Logger.error("internal node not resolved")
}


object IR_RangeIndex {
  def apply(indices : IR_Range*) = new IR_RangeIndex(indices.toArray)
}

case class IR_RangeIndex(var indices : Array[IR_Range]) extends IR_Index  {
  override def length() : Int = indices.length
  override def toExpressionIndex : IR_ExpressionIndex = Logger.error("not implemented")
  override def +(that : IR_Index) : IR_Index =  Logger.error("not implemented")
  override def -(that : IR_Index) : IR_Index =  Logger.error("not implemented")
  override def datatype : IR_Datatype = Logger.error("not implemented")
  override def prettyprint(out : PpStream) : Unit = Logger.error("internal node not resolved")
}

case class IR_MatIndex(indices : Array[IR_Index]) extends IR_Index {
  def y : IR_Index = {

    indices(0)
  }

  def x : Option[IR_Index] = {
    if (indices.length == 1) None
    else {

      Some(indices(1))
    }
  }

  def asInt : Int = {
    indices(0) match {
      case _ : IR_ExpressionIndex => Logger.error("Index is an expression!")
      case cidx : IR_ConstIndex => cidx.indices(0)
      case _ : IR_RangeIndex => Logger.error("Index is a range!")
    }
  }

  override def length(): Int = indices.length

  override def toExpressionIndex: IR_ExpressionIndex = ???

  override def +(that: IR_Index): IR_Index = ???

  override def -(that: IR_Index): IR_Index = ???

  override def datatype: IR_Datatype = ???

  override def prettyprint(out: PpStream): Unit = {
    out << indices(0)
    if(indices.length == 2) out << indices(1)
  }
}