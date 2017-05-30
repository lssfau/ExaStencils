package exastencils.operator.l3

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.operator.l4.L4_OperatorTimesField
import exastencils.prettyprinting.PpStream

// TODO: is it really necessary to wrap convolutions in separate nodes?

/// L3_OperatorTimesField

case class L3_OperatorTimesField(var lhs : L3_OperatorAccess, var rhs : L3_FieldAccess) extends L3_Expression {
  def prettyprint(out : PpStream) = out << lhs << " * " << rhs
  def progress = L4_OperatorTimesField(lhs.progress, rhs.progress)
}

/// L3_ResolveOperatorTimesField

object L3_ResolveOperatorTimesField extends DefaultStrategy("Resolving L3 operator field convolutions") {
  this += new Transformation("Resolve", {
    // FIXME: traverse and match operand list -> register as multiplication's member function receiving a lambda?
    case mult @ L3_Multiplication(args) if 2 == args.size =>
      (args(0), args(1)) match {
        case (lhs : L3_OperatorAccess, rhs : L3_FieldAccess) => L3_OperatorTimesField(lhs, rhs)
        case _                                               => mult
      }
  })
}
