package exastencils.baseExt.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3.L3_Expression
import exastencils.base.l4.L4_Expression
import exastencils.baseExt.l4.L4_ComplexExpression
import exastencils.prettyprinting.PpStream

case class L3_ComplexExpression(real : L3_Expression, imag : L3_Expression ) extends L3_Expression {
  override def progress : L4_Expression = ProgressLocation(L4_ComplexExpression(real.progress, imag.progress))
  override def prettyprint(out : PpStream) : Unit = out << real.prettyprint(out) << " + " << imag.prettyprint(out) << "i"
}
