package exastencils.baseExt.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_HigherDimSelection
import exastencils.prettyprinting.PpStream

/// L3_HigherDimSelection

case class L3_HigherDimSelection(var base : L3_Expression, var index : L3_ConstIndex) extends L3_Expression {
  override def prettyprint(out : PpStream) = out << base << index.map('[' + _.toString + ']').mkString("")
  override def progress : L4_HigherDimSelection = ProgressLocation(L4_HigherDimSelection(base.progress, index.progress))
}
