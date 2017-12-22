package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_Assignment

// TODO: use specialized compound assignment and eliminate op member
case class L4_Assignment(var dest : L4_Access, var src : L4_Expression, var op : String, var condition : Option[L4_Expression] = None) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << dest << ' ' << op << ' ' << src
  override def progress = ProgressLocation(IR_Assignment(dest.progress, src.progress, op))
}

/// L4_CompoundAssignment

case class L4_CompoundAssignment(var dest : L4_Expression, var src : L4_Expression, var op : L4_BinaryOperators.BinaryOperators, var condition : Option[L4_Expression] = None) extends L4_Statement {
  Logger.warn("Not fully incorporated yet")
  override def prettyprint(out : PpStream) : Unit = out << dest << ' ' << op << "=" << ' ' << src
  override def progress = ProgressLocation(IR_CompoundAssignment(dest.progress, src.progress, L4_BinaryOperators.progress(op)))
}
