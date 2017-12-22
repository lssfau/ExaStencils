package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.l4._
import exastencils.prettyprinting.PpStream

/// L4_HigherDimSelection

case class L4_HigherDimSelection(var base : L4_Expression, var index : L4_ConstIndex) extends L4_Expression {
  override def prettyprint(out : PpStream) = out << base << index.map('[' + _.toString + ']').mkString("")
  override def progress : IR_HighDimAccess = ProgressLocation(IR_HighDimAccess(base.progress, index.progress))
}
