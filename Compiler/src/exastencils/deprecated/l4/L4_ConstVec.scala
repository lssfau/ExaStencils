package exastencils.deprecated.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.base.l4.L4_Expression
import exastencils.prettyprinting._

// TODO: integrate with the new type system

@deprecated("to be integrated with the new type system/ vector classes")
trait L4_ConstVec extends L4_Expression {
  override def progress : IR_ExpressionIndex
}

@deprecated("to be integrated with the new type system/ vector classes")
case class L4_ConstVec1D(var x : Double) extends L4_ConstVec {
  def prettyprint(out : PpStream) = { out << "[ " << x << " ]" }
  override def progress : IR_ExpressionIndex = ???
}

@deprecated("to be integrated with the new type system/ vector classes")
case class L4_ConstVec2D(var x : Double, var y : Double) extends L4_ConstVec {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << " ]" }
  override def progress : IR_ExpressionIndex = ???
}

@deprecated("to be integrated with the new type system/ vector classes")
case class L4_ConstVec3D(var x : Double, var y : Double, var z : Double) extends L4_ConstVec {
  def prettyprint(out : PpStream) = { out << "[ " << x << ", " << y << ", " << z << " ]" }
  override def progress : IR_ExpressionIndex = ???
}
