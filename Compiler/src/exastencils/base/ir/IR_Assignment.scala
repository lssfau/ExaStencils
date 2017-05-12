package exastencils.base.ir

import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// IR_Assignment

// TODO: use specialized compound assignment and eliminate op member
case class IR_Assignment(var dest : IR_Expression, var src : IR_Expression, var op : String = "=") extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << dest << ' ' << op << ' ' << src << ';'
}

/// IR_CompoundAssignment

case class IR_CompoundAssignment(var dest : IR_Expression, var src : IR_Expression, var op : IR_BinaryOperators.BinaryOperators) extends IR_Statement {
  Logger.warn("Not fully incorporated yet")
  override def prettyprint(out : PpStream) : Unit = out << dest << ' ' << op << "=" << ' ' << src << ';'
}
