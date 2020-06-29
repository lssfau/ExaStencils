package exastencils.baseExt.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3.L3_Addition
import exastencils.base.l3.L3_Expression
import exastencils.base.l3.L3_Subtraction
import exastencils.baseExt.l4.L4_ComplexExpression
import exastencils.prettyprinting.PpStream

/*
case class L3_ComplexExpression(real : L3_Expression, op : Boolean, imag : L3_Expression ) extends L3_Expression {
  override def progress : L4_ComplexExpression = ProgressLocation(L4_ComplexExpression(real.progress,op,imag.progress))
//  override def prettyprint(out : PpStream) : Unit = out << "complex(" << real << "," << imag << ")"
//  override def prettyprint(out : PpStream) : Unit = out << "(" << real << "+" << imag << "i)"
  override def prettyprint(out : PpStream) : Unit = out << real << (if(op) "+" else "-") << imag << "i"
}
*/
object L3_ComplexExpression  {
  def apply(expr : L3_Expression) = {
    expr match {
      case add : L3_Addition => new L3_ComplexExpression(add.summands(0),true,add.summands(1))
      case sub : L3_Subtraction => new L3_ComplexExpression(sub.left, false, sub.right)
    }
  }
}
 case class L3_ComplexExpression(real : L3_Expression, op : Boolean, imag : L3_Expression ) extends L3_Expression {
  override def progress : L4_ComplexExpression = ProgressLocation(L4_ComplexExpression(real.progress,op,imag.progress))
  //  override def prettyprint(out : PpStream) : Unit = out << "complex(" << real << "," << imag << ")"
  //  override def prettyprint(out : PpStream) : Unit = out << "(" << real << "+" << imag << "i)"
  override def prettyprint(out : PpStream) : Unit = out << "(" << real << (if(op) "+" else "-") << imag << "i)"
}