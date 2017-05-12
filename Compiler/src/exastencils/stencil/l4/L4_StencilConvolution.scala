package exastencils.stencil.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldAccess
import exastencils.operator.l4.L4_StencilAccess
import exastencils.prettyprinting.PpStream
import exastencils.stencil.ir._

// TODO: is it really necessary to wrap convolutions in separate nodes?

/// L4_StencilConvolution

case class L4_StencilConvolution(var left : L4_StencilAccess, var right : L4_FieldAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << left << " * " << right
  def progress = IR_StencilConvolution(left.progress.asInstanceOf[IR_StencilAccess], right.progress)
}

/// L4_StencilFieldConvolution

case class L4_StencilFieldConvolution(var left : L4_StencilFieldAccess, var right : L4_FieldAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << left << " * " << right
  def progress = IR_StencilFieldConvolution(left.progress.asInstanceOf[IR_StencilFieldAccess], right.progress)
}

/// L4_UnresolveStencilFieldConvolutions

object L4_UnresolveStencilFieldConvolutions extends DefaultStrategy("Revert stencil field convolutions to plain multiplications") {
  this += new Transformation("Replace", {
    case L4_StencilConvolution(lhs, rhs)      => L4_Multiplication(lhs, rhs)
    case L4_StencilFieldConvolution(lhs, rhs) => L4_Multiplication(lhs, rhs)
  })
}
