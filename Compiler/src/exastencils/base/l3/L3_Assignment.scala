package exastencils.base.l3

import exastencils.base.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L3_Assignment

// TODO: use specialized compound assignment and eliminate op member
case class L3_Assignment(var dest : L3_Access, var src : L3_Expression, var op : String, var condition : Option[L3_Expression]) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << dest << ' ' << op << ' ' << src << '\n'
  override def progress = L4_Assignment(dest.progress, src.progress, op, L3_ProgressOption(condition)(_.progress))
}

/// L3_CompoundAssignment

case class L3_CompoundAssignment(var dest : L3_Expression, var src : L3_Expression, var op : L3_BinaryOperators.BinaryOperators, var condition : Option[L3_Expression]) extends L3_Statement {
  Logger.warn("Not fully incorporated yet")
  override def prettyprint(out : PpStream) = out << dest << ' ' << op << "=" << ' ' << src
  override def progress = L4_CompoundAssignment(dest.progress, src.progress, L3_BinaryOperators.progress(op), L3_ProgressOption(condition)(_.progress))
}
