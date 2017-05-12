package exastencils.stencil.l4

import exastencils.base.l4.L4_Expression
import exastencils.operator.l4.L4_StencilAccess
import exastencils.prettyprinting.PpStream
import exastencils.stencil.ir._

// TODO: is it really necessary to wrap convolutions in separate nodes?

/// L4_StencilStencilConvolution

case class L4_StencilStencilConvolution(var left : L4_StencilAccess, var right : L4_StencilAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << left << " * " << right
  def progress = IR_StencilStencilConvolution(left.progress.asInstanceOf[IR_StencilAccess], right.progress.asInstanceOf[IR_StencilAccess])
}

/// L4_StencilFieldStencilConvolution

case class L4_StencilFieldStencilConvolution(var left : L4_StencilFieldAccess, var right : L4_StencilAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << left << " * " << right
  def progress = IR_StencilFieldStencilConvolution(left.progress.asInstanceOf[IR_StencilFieldAccess], right.progress.asInstanceOf[IR_StencilAccess])
}
