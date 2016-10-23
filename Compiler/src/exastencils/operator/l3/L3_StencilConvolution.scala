package exastencils.operator.l3

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.prettyprinting.PpStream
import exastencils.stencil.l4.L4_StencilConvolution

// TODO: is it really necessary to wrap convolutions in separate nodes?

/// L3_StencilConvolution

case class L3_StencilConvolution(var stencilAccess : L3_StencilAccess, var fieldAccess : L3_FieldAccess) extends L3_Expression {
  def prettyprint(out : PpStream) = out << stencilAccess << " * " << fieldAccess
  def progress = L4_StencilConvolution(stencilAccess.progress, fieldAccess.progress)
}

/// L3_ResolveFieldFieldConvolutions

object L3_ResolveStencilConvolutions extends DefaultStrategy("Resolving L3 stencil field convolutions") {
  this += new Transformation("Resolve", {
    // FIXME: traverse and match operand list -> register as multiplication's member function receiving a lambda?
    case mult @ L3_Multiplication(args) if 2 == args.size =>
      (args(0), args(1)) match {
        case (lhs : L3_StencilAccess, rhs : L3_FieldAccess) => L3_StencilConvolution(lhs, rhs)
        case _                                              => mult
      }
  })
}
