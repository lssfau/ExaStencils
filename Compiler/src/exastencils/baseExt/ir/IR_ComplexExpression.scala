package exastencils.baseExt.ir

import exastencils.base.ir.IR_Addition
import exastencils.base.ir.IR_ComplexDatatype
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Division
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Negative
import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_Number
import exastencils.base.ir.IR_Power
import exastencils.base.ir.IR_Root
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

object IR_ComplexExpression {
  def apply(real : IR_Expression, imag : IR_Expression): IR_ComplexExpression = {
    new IR_ComplexExpression(real,imag)
  }
}
case class IR_ComplexExpression(real : IR_Expression, imag : IR_Expression) extends IR_Expression {

  override def datatype : IR_Datatype = IR_ComplexDatatype(IR_ResultingDatatype(real.datatype, imag.datatype))
  override def prettyprint(out : PpStream) : Unit = {
    out << real
    if (sign(imag, false)) out << " + "
    out << imag << "i"
  }
  def sign(e : IR_Expression, neg : Boolean) : Boolean = {
    e match {
      case IR_Negative(x) => sign(x,!neg)
      case n : IR_Number => neg
      //TODO how to handle additions etc?
      case _ => neg
      //case x => sign(x,neg)
    }
  }
}

object IR_ComplexOperations {
  //TODO parser support
  def conjugate(c : IR_ComplexExpression): IR_ComplexExpression = {
    IR_ComplexExpression(c.real, c.imag)
  }
  def add(c1 : IR_ComplexExpression, c2 : IR_ComplexExpression): IR_ComplexExpression = {
    IR_ComplexExpression(c1.real + c2.real, c1.imag + c2.imag)
  }
  def sub(c1 : IR_ComplexExpression, c2 : IR_ComplexExpression): IR_ComplexExpression = {
    IR_ComplexExpression(c1.real - c2.real, c1.imag - c2.imag)
  }
  def mult(c1 : IR_ComplexExpression, c2 : IR_ComplexExpression): IR_ComplexExpression = {
    IR_ComplexExpression(c1.real*c2.real - c1.imag * c2.imag, c1.real*c2.imag + c1.imag*c2.real)
  }
  def reciprocal(c : IR_ComplexExpression) : IR_ComplexExpression = {
    IR_ComplexExpression(IR_Division(c.real,squareAdd(c)), IR_Negative(IR_Division(c.imag,squareAdd(c))))
  }
  def div(c1 : IR_ComplexExpression, c2 : IR_ComplexExpression): IR_ComplexExpression = {
    mult(c1,reciprocal(c2))
  }
  // why does IR_Root not extends IR_Expression?
  //TODO parser support
  def abs(c : IR_ComplexExpression) : IR_Node = {
    IR_Root(squareAdd(c))
  }

  // util
  def squareAdd(c : IR_ComplexExpression) : IR_Addition = {
    IR_Addition(IR_Power(c.real,IR_IntegerConstant(2)), IR_Power(c.imag,IR_IntegerConstant(2)))
  }
}
