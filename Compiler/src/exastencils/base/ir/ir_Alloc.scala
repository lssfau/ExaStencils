package exastencils.base.ir

import exastencils.prettyprinting.PpStream

case class IR_ArrayAllocation(var name : IR_Expression, // no string - could be an IV
    var innerDatatype : IR_Datatype,
    var size : IR_Expression) extends IR_Statement {
  //override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << name << " = " << "new" << ' ' << innerDatatype << "[" << size << "];"
}

case class IR_ArrayFree(var pointer : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "delete[] " << pointer << ";"
}
