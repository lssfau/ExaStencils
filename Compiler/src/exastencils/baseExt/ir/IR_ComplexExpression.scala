package exastencils.baseExt.ir

import exastencils.base.ir.IR_ComplexDatatype
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

case class IR_ComplexExpression(real : IR_Expression, op : Boolean, imag : IR_Expression) extends IR_Expression {
  override def datatype : IR_Datatype = IR_ComplexDatatype(IR_ResultingDatatype(real.datatype, imag.datatype))
  override def prettyprint(out : PpStream) : Unit =
    out << real << (if(op) "+" else "-") << imag << "i"
}
