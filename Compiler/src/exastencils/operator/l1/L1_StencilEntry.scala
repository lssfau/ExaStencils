package exastencils.operator.l1

import exastencils.base.l1._
import exastencils.operator.l2._
import exastencils.prettyprinting._

/// L1_StencilOffsetEntry

case class L1_StencilEntry(var offset : L1_ConstIndex, var coefficient : L1_Expression) extends L1_Node with L1_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
  override def progress = L2_StencilOffsetEntry(offset.progress, coefficient.progress)
  def numDims = offset.length
}
