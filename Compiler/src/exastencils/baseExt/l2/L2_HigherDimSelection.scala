package exastencils.baseExt.l2

import exastencils.base.l2._
import exastencils.baseExt.l3.L3_HigherDimSelection
import exastencils.prettyprinting.PpStream

/// L2_HigherDimSelection

case class L2_HigherDimSelection(var base : L2_Expression, var index : L2_ConstIndex) extends L2_Expression {
  override def prettyprint(out : PpStream) = out << base << index.map('[' + _.toString + ']').mkString("")
  override def progress : L3_HigherDimSelection = L3_HigherDimSelection(base.progress, index.progress)
}
