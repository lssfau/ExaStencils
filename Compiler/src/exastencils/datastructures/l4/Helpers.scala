package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.prettyprinting._

trait Index extends Expression {
  override def progressToIr : ir.MultiIndex = new ir.MultiIndex(extractArray)
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

trait RealIndex extends Expression {
  override def progressToIr : ir.MultiIndex
}

case class RealIndex1D(var x : Double) extends RealIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << " ]" }

  def progressToIr : ir.MultiIndex = new ir.MultiIndex(ir.FloatConstant(x))
}

case class RealIndex2D(var x : Double, var y : Double) extends RealIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << " ]" }

  def progressToIr : ir.MultiIndex = new ir.MultiIndex(ir.FloatConstant(x), ir.FloatConstant(y))
}

case class RealIndex3D(var x : Double, var y : Double, var z : Double) extends RealIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << ", " << z << " ]" }

  def progressToIr : ir.MultiIndex = new ir.MultiIndex(ir.FloatConstant(x), ir.FloatConstant(y), ir.FloatConstant(z))
}

trait ExpressionIndex extends Expression {
  override def progressToIr : ir.MultiIndex
}

case class ExpressionIndex1D(var x : Expression) extends ExpressionIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << " ]" }

  def progressToIr : ir.MultiIndex = new ir.MultiIndex(Array(x.progressToIr))
}

case class ExpressionIndex2D(var x : Expression, var y : Expression) extends ExpressionIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << " ]" }

  def progressToIr : ir.MultiIndex = new ir.MultiIndex(Array(x.progressToIr, y.progressToIr))
}

case class ExpressionIndex3D(var x : Expression, var y : Expression, var z : Expression) extends ExpressionIndex {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << ", " << z << " ]" }

  def progressToIr : ir.MultiIndex = new ir.MultiIndex(Array(x.progressToIr, y.progressToIr, z.progressToIr))
}

abstract class ComponentIndex extends Node with PrettyPrintable with ProgressableToIr {
  def toMultiIndexArray : Array[ir.MultiIndex]
  override def progressToIr : Array[ir.ConstIndex] = ???
}

case class ComponentIndex1d(var begin : Option[Int], var end : Option[Int]) extends ComponentIndex {
  def this(a : Int, b : Int) = this(Some(a), Some(b))
  def this(idx : Index1D) = this(idx.x, idx.x)

  def prettyprintInner(out : PpStream) = {
    if (begin.isDefined) out << begin.get
    out << ':'
    if (end.isDefined) out << end.get
  }

  override def prettyprint(out : PpStream) = {
    out << '['
    prettyprintInner(out)
    out << ']'
  }
  override def toMultiIndexArray = ???
  override def progressToIr : Array[ir.ConstIndex] = ???
}

case class ComponentIndex2d(var x : ComponentIndex1d, var y : ComponentIndex1d) extends ComponentIndex {
  def this(idx : Index2D) = this(new ComponentIndex1d(idx.x, idx.x), new ComponentIndex1d(idx.y, idx.y))

  override def prettyprint(out : PpStream) = {
    out << '[';
    x.prettyprintInner(out)
    out << ',';
    y.prettyprintInner(out)
    out << ']'
  }
  override def toMultiIndexArray = ???
  override def progressToIr : Array[ir.ConstIndex] = ???
}
