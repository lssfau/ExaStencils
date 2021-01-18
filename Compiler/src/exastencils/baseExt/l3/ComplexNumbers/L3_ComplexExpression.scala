package exastencils.baseExt.l3.ComplexNumbers

import exastencils.base.ProgressLocation
import exastencils.base.l3.L3_Expression
import exastencils.baseExt.l4.ComplexNumbers
import exastencils.baseExt.l4.ComplexNumbers.L4_ComplexExpression
import exastencils.prettyprinting.PpStream

case class L3_ComplexExpression(real : L3_Expression, op : Boolean, imag : L3_Expression ) extends L3_Expression {
 override def progress : L4_ComplexExpression = ProgressLocation(ComplexNumbers.L4_ComplexExpression(real.progress,op,imag.progress))
 // no bracket version
 //override def prettyprint(out : PpStream) : Unit = out <<  real << (if(op) "+" else "-") << imag << "j"

 // bracket version
 override def prettyprint(out : PpStream) : Unit = out << "(" << real << (if(op) "+" else "-") << imag << "j)"

 // constructor version
 //override def prettyprint(out : PpStream) : Unit = out << "complex(" << real << "," << imag << ")"
}
