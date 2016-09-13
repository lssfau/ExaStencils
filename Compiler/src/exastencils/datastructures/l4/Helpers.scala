package exastencils.datastructures.l4

import exastencils.base.ir._
import exastencils.base.l4.L4_Expression
import exastencils.prettyprinting._

// TODO: integrate with the new type system

trait ConstVec extends L4_Expression {
  override def progress : IR_ExpressionIndex
}

case class ConstVec1D(var x : Double) extends ConstVec {
  def prettyprint(out : PpStream) = { out << "[ " << x << " ]" }

  def progress : IR_ExpressionIndex = ???
}

case class ConstVec2D(var x : Double, var y : Double) extends ConstVec {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << " ]" }

  def progress : IR_ExpressionIndex = ???
}

case class ConstVec3D(var x : Double, var y : Double, var z : Double) extends ConstVec {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << ", " << z << " ]" }

  def progress : IR_ExpressionIndex = ???
}



