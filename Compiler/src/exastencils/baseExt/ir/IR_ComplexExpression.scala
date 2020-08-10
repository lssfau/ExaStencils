package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir
import exastencils.base.ir.IR_Addition
import exastencils.base.ir.IR_BoundedScalar
import exastencils.base.ir.IR_ComplexDatatype
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Division
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_Negative
import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_Number
import exastencils.base.ir.IR_PlainFunction
import exastencils.base.ir.IR_Power
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Root
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_Subtraction
import exastencils.base.ir.IR_VariableAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

object IR_ComplexExpression {
  def apply(real : IR_Expression,op : Boolean, imag : IR_Expression): IR_ComplexExpression = {
    new IR_ComplexExpression(real,op,imag)
  }
}
case class IR_ComplexExpression(real : IR_Expression, op : Boolean, imag : IR_Expression) extends IR_Expression {
  override def datatype : IR_Datatype = IR_ComplexDatatype(IR_ResultingDatatype(real.datatype, imag.datatype))
  override def prettyprint(out : PpStream) : Unit = {
    real.datatype match {
      case IR_RealDatatype | IR_FloatDatatype | IR_DoubleDatatype =>
      case _ => Logger.error("Need to resolve real part to floating point expression")
    }
    imag.datatype match {
      case IR_RealDatatype | IR_FloatDatatype | IR_DoubleDatatype | IR_IntegerDatatype if(imag.isInstanceOf[IR_Number]) =>
      case _                                                                              => Logger.error("Need to resolve imag part to floating or integer constant")
    }
    out << "std::complex<double>(" << real << (if(op) " + " else " - ") << imag << "i)"
  }

  /*
  def resolveSign(c : IR_ComplexExpression, neg : Boolean) : Boolean = {
    imag match {
      case IR_Negative(x) => resolveSign(!neg)
      case n : IR_Number => neg
      //TODO how to handle additions etc?
      case _ => neg
      //case x => sign(x,neg)
    }
  }
*/

  def signToImag(): IR_ComplexExpression = {
    if(op) this
    else IR_ComplexExpression(real, true, IR_Negative(imag))
  }
}

object IR_ComplexOperations {
  //TODO parser support
  def conjugate(c : IR_ComplexExpression): IR_ComplexExpression = {
    IR_ComplexExpression(c.real, !c.op, c.imag)
  }

  def add(c1 : IR_ComplexExpression, c2 : IR_ComplexExpression): IR_ComplexExpression = {
    c1.signToImag()
    c2.signToImag()
    IR_ComplexExpression(c1.real + c2.real, true, c1.imag + c2.imag)
  }
  def sub(c1 : IR_ComplexExpression, c2 : IR_ComplexExpression): IR_ComplexExpression = {
    c1.signToImag()
    c2.signToImag()
    IR_ComplexExpression(c1.real - c2.real, true, c1.imag - c2.imag)
  }
  def mult(c1 : IR_ComplexExpression, c2 : IR_ComplexExpression): IR_ComplexExpression = {
    c1.signToImag()
    c2.signToImag()
    IR_ComplexExpression(c1.real*c2.real - c1.imag * c2.imag, true, c1.real*c2.imag + c1.imag*c2.real)
  }
  def mult(sc : IR_BoundedScalar, c : IR_ComplexExpression): IR_ComplexExpression = {
    IR_ComplexExpression(c.real * sc, true, c.imag * sc)
  }
  def reciprocal(c : IR_ComplexExpression) : IR_ComplexExpression = {
    c.signToImag()
    IR_ComplexExpression(ir.IR_Division(c.real,squareAdd(c)), c.op, IR_Negative(ir.IR_Division(c.imag,squareAdd(c))))
  }
  def div(c1 : IR_ComplexExpression, c2 : IR_ComplexExpression): IR_ComplexExpression = {
    mult(c1,reciprocal(c2))
  }
  // why does IR_Root not extends IR_Expression?
  //TODO parser support
  def abs(c : IR_ComplexExpression) : IR_Node = {
    c.signToImag()
    IR_Root(squareAdd(c))
  }

  // util
  def squareAdd(c : IR_ComplexExpression) : IR_Addition = {
    IR_Addition(IR_Power(c.real,IR_IntegerConstant(2)), IR_Power(c.imag,IR_IntegerConstant(2)))
  }

  def generateOMPRedAdd(dt : IR_Datatype) : IR_PlainFunction = {
    val body = ListBuffer[IR_Statement]()
    val OMP_out = IR_VariableAccess("omp_out", dt)
    val OMP_in = IR_VariableAccess("omp_in", dt)
    body += ir.IR_Assignment(OMP_out,OMP_in, "+=")
    val args = ListBuffer[IR_FunctionArgument]()
    args += IR_FunctionArgument(OMP_out)
    args += IR_FunctionArgument(OMP_in)
    IR_PlainFunction("ComplexRedAdd", dt, args, body)
  }
}



object IR_ResolveComplexOps extends DefaultStrategy("Resolve operations with complex expressions") {
    this += new Transformation("resolve", {
      case _ @ IR_Addition(args) if(args.forall(a => a.isInstanceOf[IR_ComplexExpression])) =>
          if(args.length != 2) Logger.error("addition with more than 2 ces currently not supported")
        IR_ComplexOperations.add(args(0).asInstanceOf[IR_ComplexExpression], args(1).asInstanceOf[IR_ComplexExpression])
      case _ @ IR_Subtraction(left,right) if(left.isInstanceOf[IR_ComplexExpression] && right.isInstanceOf[IR_ComplexExpression]) =>
        IR_ComplexOperations.sub(left.asInstanceOf[IR_ComplexExpression],right.asInstanceOf[IR_ComplexExpression])
      case _ @ IR_Multiplication(args) if(args.forall(a => a.isInstanceOf[IR_ComplexExpression])) =>
        if(args.length != 2) Logger.error("multiplication with more than 2 ces currently not supported")
        IR_ComplexOperations.mult(args(0).asInstanceOf[IR_ComplexExpression], args(1).asInstanceOf[IR_ComplexExpression])
      case _ @ IR_Division(ce1,ce2) if(ce1.isInstanceOf[IR_ComplexExpression] && ce2.isInstanceOf[IR_ComplexExpression]) =>
        IR_ComplexOperations.div(ce1.asInstanceOf[IR_ComplexExpression],ce2.asInstanceOf[IR_ComplexExpression])
    })
}


