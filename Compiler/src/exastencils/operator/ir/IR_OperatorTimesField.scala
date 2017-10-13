package exastencils.operator.ir

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.prettyprinting.PpStream

/// IR_OperatorTimesField

case class IR_OperatorTimesField(var lhs : IR_OperatorAccess, var rhs : IR_FieldAccess) extends IR_Expression {
  override def datatype = rhs.datatype
  override def prettyprint(out : PpStream) = out << lhs << " * " << rhs
}

/// IR_ResolveOperatorTimesField

object IR_ResolveOperatorTimesField extends DefaultStrategy("Resolving IR operator field convolutions") {
  this += new Transformation("Resolve", {
    // FIXME: traverse and match operand list -> register as multiplication's member function receiving a lambda?
    case mult @ IR_Multiplication(args) if 2 == args.size =>
      (args(0), args(1)) match {
        case (lhs : IR_OperatorAccess, rhs : IR_FieldAccess) => IR_OperatorTimesField(lhs, rhs)
        case _                                               => mult
      }
  })
}

/// IR_UnresolveOperatorTimesField

object IR_UnresolveOperatorTimesField extends DefaultStrategy("Revert stencil field convolutions to plain multiplications") {
  this += new Transformation("Replace", {
    case IR_OperatorTimesField(lhs, rhs) => IR_Multiplication(lhs, rhs)
  })
}
