package exastencils.datastructures.l4

import exastencils.base.l4.L4_Expression
import exastencils.datastructures._
import exastencils.prettyprinting._

trait Index extends L4_Expression {
  override def progress : ir.MultiIndex = new ir.MultiIndex(extractArray)
  def apply(i : Int) : Int
  def extractArray : Array[Int]
}

case class Index1D(var x : Int) extends Index {
  def prettyprint(out : PpStream) = { out << "[ " << x << " ]" }

  def extractArray = Array(x)

  def apply(i : Int) : Int = {
    i match {
      case 0 => x
    }
  }
}

case class Index2D(var x : Int, var y : Int) extends Index {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << " ]" }

  def extractArray = Array(x, y)

  def apply(i : Int) : Int = {
    i match {
      case 0 => x
      case 1 => y
    }
  }
}

case class Index3D(var x : Int, var y : Int, var z : Int) extends Index {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << ", " << z << " ]" }

  def extractArray = Array(x, y, z)

  def apply(i : Int) : Int = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
    }
  }
}

trait RealIndex extends L4_Expression {
  override def progress : ir.MultiIndex
}

case class RealIndex1D(var x : Double) extends RealIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << " ]" }

  def progress : ir.MultiIndex = new ir.MultiIndex(ir.FloatConstant(x))
}

case class RealIndex2D(var x : Double, var y : Double) extends RealIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << " ]" }

  def progress : ir.MultiIndex = new ir.MultiIndex(ir.FloatConstant(x), ir.FloatConstant(y))
}

case class RealIndex3D(var x : Double, var y : Double, var z : Double) extends RealIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << ", " << z << " ]" }

  def progress : ir.MultiIndex = new ir.MultiIndex(ir.FloatConstant(x), ir.FloatConstant(y), ir.FloatConstant(z))
}

trait ExpressionIndex extends L4_Expression {
  override def progress : ir.MultiIndex
}

case class ExpressionIndex1D(var x : L4_Expression) extends ExpressionIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << " ]" }

  def progress : ir.MultiIndex = new ir.MultiIndex(Array(x.progress))
}

case class ExpressionIndex2D(var x : L4_Expression, var y : L4_Expression) extends ExpressionIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << " ]" }

  def progress : ir.MultiIndex = new ir.MultiIndex(Array(x.progress, y.progress))
}

case class ExpressionIndex3D(var x : L4_Expression, var y : L4_Expression, var z : L4_Expression) extends ExpressionIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << ", " << z << " ]" }

  def progress : ir.MultiIndex = new ir.MultiIndex(Array(x.progress, y.progress, z.progress))
}
