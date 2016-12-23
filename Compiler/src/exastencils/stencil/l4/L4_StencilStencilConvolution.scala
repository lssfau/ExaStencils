package exastencils.stencil.l4

import exastencils.base.l4.L4_Expression
import exastencils.operator.l4.L4_StencilAccess
import exastencils.prettyprinting.PpStream
import exastencils.stencil.ir._

// TODO: is it really necessary to wrap convolutions in separate nodes?

/// L4_StencilStencilConvolution

case class L4_StencilStencilConvolution(var stencilLeft : L4_StencilAccess, var stencilRight : L4_StencilAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << stencilLeft << " * " << stencilRight
  def progress = IR_StencilStencilConvolution(stencilLeft.target.getProgressedObject, stencilRight.target.getProgressedObject)
}

/// L4_StencilFieldStencilConvolution

case class L4_StencilFieldStencilConvolution(var stencilLeft : L4_StencilFieldAccess, var stencilRight : L4_StencilAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << stencilLeft << " * " << stencilRight
  def progress = IR_StencilFieldStencilConvolution(stencilLeft.progress.asInstanceOf[IR_StencilFieldAccess], stencilRight.target.getProgressedObject)
}
