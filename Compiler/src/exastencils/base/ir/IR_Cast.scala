package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// IR_Cast

case class IR_Cast(var datatype : IR_Datatype, var toCast : IR_Expression) extends IR_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "((" << datatype << ")" << toCast << ")"
}

case class IR_ToInt(var toCase : IR_Expression) extends IR_Expression with IR_Expandable {
  override def datatype = IR_IntegerDatatype
  override def expand() = IR_Cast(IR_IntegerDatatype, IR_FunctionCall("floor", toCase))
}
