package exastencils.datastructures.l4

import scala.collection.mutable.HashSet
import exastencils.datastructures._

case class TempOption(val key : String, val value : String) extends Annotatable

trait Index extends Expression {
  override def progressToIr : ir.MultiIndex
}

case class Index2D(var x : Int, var y : Int) extends Index {
  def progressToIr : ir.MultiIndex = new ir.MultiIndex(Array(x, y))
}

case class Index3D(var x : Int, var y : Int, var z : Int) extends Index {
  def progressToIr : ir.MultiIndex = new ir.MultiIndex(Array(x, y, z))
}

trait ExpressionIndex extends Expression {
  override def progressToIr : ir.MultiIndex
}

case class ExpressionIndex2D(var x : Expression, var y : Expression) extends ExpressionIndex {
  def progressToIr : ir.MultiIndex = new ir.MultiIndex(Array(x.progressToIr, y.progressToIr))
}

case class ExpressionIndex3D(var x : Expression, var y : Expression, var z : Expression) extends ExpressionIndex {
  def progressToIr : ir.MultiIndex = new ir.MultiIndex(Array(x.progressToIr, y.progressToIr, z.progressToIr))
}
