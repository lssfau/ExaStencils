package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.prettyprinting._

/// L4_Reduction

// FIXME: target as access -> resolve datatype
// FIXME: op as BinOp
case class L4_Reduction(var op : String, var target : String) extends L4_Node with L4_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "reduction ( " << op << " : " << target << " )"
  // FIXME: IR_RealDatatype
  override def progress = ProgressLocation(IR_Reduction(op, IR_VariableAccess(target, IR_RealDatatype)))
}
