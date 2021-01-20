package exastencils.baseExt.ir.ComplexNumbers

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_Expression
import exastencils.prettyprinting.PpStream

case class IR_ComplexNumberAccessImag(name : String) extends IR_Expression {
  override def datatype : IR_Datatype = IR_DoubleDatatype
  override def prettyprint(out : PpStream) : Unit = out << name << ".imag()"
}

case class IR_ComplexNumberAccessReal(name : String) extends IR_Expression {
  override def datatype : IR_Datatype = IR_DoubleDatatype
  override def prettyprint(out : PpStream) : Unit = out << name << ".real()"
}
