package exastencils.field.l3

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldFieldConvolution
import exastencils.prettyprinting.PpStream

/// L3_FieldFieldConvolution

case class L3_FieldFieldConvolution(var lhs : L3_FieldAccess, var rhs : L3_FieldAccess) extends L3_Expression {
  override def prettyprint(out : PpStream) = out << lhs << " * " << rhs
  override def progress = L4_FieldFieldConvolution(lhs.progress, rhs.progress)
}

/// L3_ResolveFieldFieldConvolutions

object L3_ResolveFieldFieldConvolutions extends DefaultStrategy("Resolving L3 field field convolutions") {
  this += new Transformation("Resolve", {
    // FIXME: traverse and match operand list -> register as multiplication's member function receiving a lambda?
    case mult @ L3_Multiplication(args) if 2 == args.size =>
      (args(0), args(1)) match {
        case (lhs : L3_FieldAccess, rhs : L3_FieldAccess) => L3_FieldFieldConvolution(lhs, rhs)
        case _                                            => mult
      }
  })
}
