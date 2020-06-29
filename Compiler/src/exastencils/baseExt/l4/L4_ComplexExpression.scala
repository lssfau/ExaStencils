package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Addition
import exastencils.base.l4.L4_Expression
import exastencils.base.l4.L4_Subtraction
import exastencils.baseExt.ir.IR_ComplexExpression
import exastencils.prettyprinting.PpStream

/*
case class L4_ComplexExpression(real : L4_Expression, op : Boolean, imag : L4_Expression) extends L4_Expression {
  override def progress : IR_ComplexExpression = ProgressLocation(IR_ComplexExpression(real.progress,op, imag.progress))
  //override def prettyprint(out : PpStream) : Unit = out << "complex(" << real << "," <<  imag << ")"
  //override def prettyprint(out : PpStream) : Unit = out << "(" << real << "+" << imag << "i)"
  override def prettyprint(out : PpStream) : Unit = out << real << (if(op) "+" else "-") << imag << "i"
}
*/
object L4_ComplexExpression {
  def apply(expr : L4_Expression) = {
    expr match {
      case add : L4_Addition   => new L4_ComplexExpression(add.summands(0), true, add.summands(1))
      case sub : L4_Subtraction => new L4_ComplexExpression(sub.left, false, sub.right)
    }
  }
}
case class L4_ComplexExpression(real : L4_Expression, op : Boolean, imag : L4_Expression) extends L4_Expression {
  override def progress : IR_ComplexExpression = ProgressLocation(IR_ComplexExpression(real.progress,op, imag.progress))
  //override def prettyprint(out : PpStream) : Unit = out << "complex(" << real << "," <<  imag << ")"
  //override def prettyprint(out : PpStream) : Unit = out << "(" << real << "+" << imag << "i)"
  override def prettyprint(out : PpStream) : Unit = out << "(" << real << (if(op) "+" else "-") << imag << "i)"
}