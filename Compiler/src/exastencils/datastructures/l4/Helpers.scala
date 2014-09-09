package exastencils.datastructures.l4

import exastencils.datastructures._

trait Index extends Expression {
  override def progressToIr : ir.MultiIndex
  def apply(i : Int) : Int
}

case class Index2D(var x : Int, var y : Int) extends Index {
  def progressToIr : ir.MultiIndex = new ir.MultiIndex(Array(x, y))

  def apply(i : Int) : Int = {
    i match {
      case 0 => x
      case 1 => y
    }
  }
}

case class Index3D(var x : Int, var y : Int, var z : Int) extends Index {
  def progressToIr : ir.MultiIndex = new ir.MultiIndex(Array(x, y, z))

  def apply(i : Int) : Int = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
    }
  }
}

trait RealIndex extends Expression {
  override def progressToIr : ir.MultiIndex
}

case class RealIndex2D(var x : Double, var y : Double) extends RealIndex {
  def progressToIr : ir.MultiIndex = new ir.MultiIndex(ir.FloatConstant(x), ir.FloatConstant(y))
}

case class RealIndex3D(var x : Double, var y : Double, var z : Double) extends RealIndex {
  def progressToIr : ir.MultiIndex = new ir.MultiIndex(ir.FloatConstant(x), ir.FloatConstant(y), ir.FloatConstant(z))
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
