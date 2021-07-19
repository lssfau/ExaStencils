package exastencils.baseExt.ir.ComplexNumbers

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_BooleanDatatype
import exastencils.base.ir.IR_ComplexDatatype
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_VariableAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

object IR_ComplexNumberAccessImag  {
  def apply(args : ListBuffer[IR_Expression]) : IR_ComplexNumberAccessImag = {
    args(0) match {
      case va : IR_VariableAccess if(va.datatype.isInstanceOf[IR_ComplexDatatype]) => IR_ComplexNumberAccessImag(va.name)
      case _ => Logger.error("unexpected argument")
    }
  }
}
case class IR_ComplexNumberAccessImag(name : String) extends IR_Expression {
  override def datatype : IR_Datatype = IR_DoubleDatatype
  override def prettyprint(out : PpStream) : Unit = out << name << ".imag()"
}
object IR_ComplexNumberAccessReal  {
  def apply(args : ListBuffer[IR_Expression]) : IR_ComplexNumberAccessReal = {
    args(0) match {
      case va : IR_VariableAccess if(va.datatype.isInstanceOf[IR_ComplexDatatype]) => IR_ComplexNumberAccessReal(va.name)
      case _ => Logger.error("unexpected argument")
    }
  }
}
case class IR_ComplexNumberAccessReal(name : String) extends IR_Expression {
  override def datatype : IR_Datatype = IR_DoubleDatatype
  override def prettyprint(out : PpStream) : Unit = out << name << ".real()"
}
object IR_ComplexNumberEqual  {
  def apply(args : ListBuffer[IR_Expression]) : IR_ComplexNumberEqual = {
      IR_ComplexNumberEqual(args(0), args(1))
  }
}
case class IR_ComplexNumberEqual(left : IR_Expression, right : IR_Expression) extends IR_Expression {
  override def datatype : IR_Datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << "((fabs(real(" << left << ") - real(" << right << ")) < 0.000000000001) && (fabs(imag(" << left << ") - imag(" << right << ")) < 0.0000000000001))"
}
object IR_ComplexNumberNotEqual  {
  def apply(args : ListBuffer[IR_Expression]) : IR_ComplexNumberNotEqual = {
    IR_ComplexNumberNotEqual(args(0), args(1))
  }
}
case class IR_ComplexNumberNotEqual(left : IR_Expression, right : IR_Expression) extends IR_Expression {
  override def datatype : IR_Datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << "((fabs(real(" << left << ") - real(" << right << ")) > 0.000000000001) || (fabs(imag(" << left << ") - imag(" << right << ")) > 0.0000000000001))"
}
object IR_ComplexNumberPolar  {
  def apply(args : ListBuffer[IR_Expression]) : IR_ComplexNumberPolar = {
    IR_ComplexNumberPolar(args(0), args(1))
  }
}
case class IR_ComplexNumberPolar(magnitude : IR_Expression, angle : IR_Expression) extends  IR_Expression {
  override def datatype : IR_Datatype = IR_ComplexDatatype(IR_DoubleDatatype)
  override def prettyprint(out : PpStream) : Unit = out << "std::polar(" << magnitude << ", " << angle << ")"
}