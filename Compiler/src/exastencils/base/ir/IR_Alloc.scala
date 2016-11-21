package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// IR_SizeOf

case class IR_SizeOf(var innerDatatype : IR_Datatype) extends IR_Expression {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "sizeof" << "(" << innerDatatype << ")"
}

/// IR_ArrayAllocation

case class IR_ArrayAllocation(var name : IR_Expression, // no string - could be an IV
    var innerDatatype : IR_Datatype,
    var size : IR_Expression) extends IR_Statement {
  //override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << name << " = " << "new" << ' ' << innerDatatype << "[" << size << "];"
}

/// IR_ArrayFree

case class IR_ArrayFree(var pointer : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "delete[] " << pointer << ";"
}
