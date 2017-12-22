package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// IR_PointerOffset

case class IR_PointerOffset(var base : IR_Expression, var offset : IR_Expression) extends IR_Expression {
  override def datatype = base.datatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << base << '+' << offset << ')'
}
