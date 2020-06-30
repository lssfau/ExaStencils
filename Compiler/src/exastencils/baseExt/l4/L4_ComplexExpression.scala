package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Expression
import exastencils.baseExt.ir.IR_ComplexExpression
import exastencils.prettyprinting.PpStream

case class L4_ComplexExpression(real : L4_Expression, op : Boolean, imag : L4_Expression) extends L4_Expression {
  override def progress : IR_ComplexExpression = ProgressLocation(IR_ComplexExpression(real.progress,op, imag.progress))
  // no bracket version
  //override def prettyprint(out : PpStream) : Unit = out << real << "+" << imag << "i"

  // bracket version
  override def prettyprint(out : PpStream) : Unit = out << "(" << real << (if(op) "+" else "-") << imag << "j)"

  // constructor version
  //override def prettyprint(out : PpStream) : Unit = out << "complex(" << real << "," << imag << ")"
}