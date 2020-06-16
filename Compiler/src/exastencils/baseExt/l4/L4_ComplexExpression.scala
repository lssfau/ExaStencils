package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_Expression
import exastencils.base.l4.L4_Expression
import exastencils.baseExt.ir.IR_ComplexExpression
import exastencils.prettyprinting.PpStream

case class L4_ComplexExpression(real : L4_Expression, imag : L4_Expression) extends L4_Expression {
  override def progress : IR_Expression = ProgressLocation(IR_ComplexExpression(real.progress, imag.progress))
  override def prettyprint(out : PpStream) : Unit = out << real.prettyprint(out) << " + " << imag.prettyprint(out) << "i"
}
