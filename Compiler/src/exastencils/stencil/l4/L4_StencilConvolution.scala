package exastencils.stencil.l4

import exastencils.base.l4.L4_Expression
import exastencils.field.l4.L4_FieldAccess
import exastencils.operator.l4.L4_StencilAccess
import exastencils.prettyprinting.PpStream
import exastencils.stencil.ir._

// TODO: is it really necessary to wrap convolutions in separate nodes?

/// L4_StencilConvolution

case class L4_StencilConvolution(var stencilAccess : L4_StencilAccess, var fieldAccess : L4_FieldAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << stencilAccess << " * " << fieldAccess
  def progress = IR_StencilConvolution(stencilAccess.target.getProgressedObject(), fieldAccess.progress)
}

/// L4_StencilFieldConvolution

case class L4_StencilFieldConvolution(var stencilFieldAccess : L4_StencilFieldAccess, var fieldAccess : L4_FieldAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << stencilFieldAccess << " * " << fieldAccess
  def progress = IR_StencilFieldConvolution(stencilFieldAccess.progress.asInstanceOf[IR_StencilFieldAccess], fieldAccess.progress)
}
