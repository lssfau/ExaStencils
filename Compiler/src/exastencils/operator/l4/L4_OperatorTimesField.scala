package exastencils.operator.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.operator.ir.IR_OperatorTimesField
import exastencils.prettyprinting.PpStream

/// L4_OperatorTimesField

case class L4_OperatorTimesField(var lhs : L4_OperatorAccess, var rhs : L4_FieldAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << lhs << " * " << rhs
  def progress = IR_OperatorTimesField(lhs.progress, rhs.progress)
}

/// L4_ResolveOperatorTimesField

object L4_ResolveOperatorTimesField extends DefaultStrategy("Resolving L4 operator field convolutions") {
  this += new Transformation("Resolve", {
    // FIXME: traverse and match operand list -> register as multiplication's member function receiving a lambda?
    case mult @ L4_Multiplication(args) if 2 == args.size =>
      (args(0), args(1)) match {
        case (lhs : L4_OperatorAccess, rhs : L4_FieldAccess) => L4_OperatorTimesField(lhs, rhs)
        case _                                               => mult
      }
  })
}

/// L4_UnresolveOperatorTimesField

object L4_UnresolveOperatorTimesField extends DefaultStrategy("Revert stencil field convolutions to plain multiplications") {
  this += new Transformation("Replace", {
    case L4_OperatorTimesField(lhs, rhs) => L4_Multiplication(lhs, rhs)
  })
}
