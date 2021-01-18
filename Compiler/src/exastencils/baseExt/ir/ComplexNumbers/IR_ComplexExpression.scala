package exastencils.baseExt.ir.ComplexNumbers

import exastencils.base.ir.IR_BooleanDatatype
import exastencils.base.ir.IR_ComplexDatatype
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_DoubleConstant
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Negative
import exastencils.base.ir.IR_RealDatatype
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

object IR_ComplexExpression {
  def apply(real : IR_Expression, op : Boolean, imag : IR_Expression): IR_ComplexExpression = {
    new IR_ComplexExpression(real, op, imag)
  }
}
case class IR_ComplexExpression(var real : IR_Expression, op : Boolean, imag : IR_Expression) extends IR_Expression {
  override def datatype : IR_Datatype = IR_ComplexDatatype(IR_ResultingDatatype(real.datatype, imag.datatype))
  override def prettyprint(out : PpStream) : Unit = {
    real.datatype match {
      case IR_IntegerDatatype if(real.isInstanceOf[IR_IntegerConstant]) =>
          real = IR_DoubleConstant(real.asInstanceOf[IR_IntegerConstant].v)
      case IR_RealDatatype | IR_FloatDatatype | IR_DoubleDatatype =>
      case _ => Logger.error("Need to resolve real part to floating point expression")
    }
    imag.datatype match {
      case IR_RealDatatype | IR_FloatDatatype | IR_DoubleDatatype | IR_IntegerDatatype  =>
      case _                                                                              => Logger.error("Need to resolve imag part to floating or integer constant")
    }
    out << "std::complex<double>(" << real << (if(op) " + " else " - ") << imag << "i)"
  }

  def signToImag(): IR_ComplexExpression = {
    if(op) this
    else IR_ComplexExpression(real, true, IR_Negative(imag))
  }
}

case class IR_ComplexNumberEqual(left : IR_Expression, right : IR_Expression) extends IR_Expression {
  override def datatype : IR_Datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << "((fabs(real(" << left << ") - real(" << right << ")) < 0.000000000001) && (fabs(imag(" << left << ") - imag(" << right << ")) < 0.0000000000001))"
}

case class IR_ComplexNumberNotEqual(left : IR_Expression, right : IR_Expression) extends IR_Expression {
  override def datatype : IR_Datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << "((fabs(real(" << left << ") - real(" << right << ")) > 0.000000000001) || (fabs(imag(" << left << ") - imag(" << right << ")) > 0.0000000000001))"
}






