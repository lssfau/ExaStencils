package exastencils.operator.l3

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.prettyprinting.PpStream
import exastencils.stencil.l4._

// TODO: is it really necessary to wrap convolutions in separate nodes?

/// L3_StencilTemplateConvolution

case class L3_StencilTemplateConvolution(var stencilAccess : L3_StencilTemplateAccess, var fieldAccess : L3_FieldAccess) extends L3_Expression {
  def prettyprint(out : PpStream) = out << stencilAccess << " * " << fieldAccess
  def progress = L4_StencilFieldConvolution(stencilAccess.progress, fieldAccess.progress)
}

/// L3_ResolveStencilTemplateConvolutions

object L3_ResolveStencilTemplateConvolutions extends DefaultStrategy("Resolving L3 stencil field convolutions") {
  this += new Transformation("Resolve", {
    // FIXME: traverse and match operand list -> register as multiplication's member function receiving a lambda?
    case mult @ L3_Multiplication(args) if 2 == args.size =>
      (args(0), args(1)) match {
        case (lhs : L3_StencilTemplateAccess, rhs : L3_FieldAccess) => L3_StencilTemplateConvolution(lhs, rhs)
        case _                                                      => mult
      }
  })
}
