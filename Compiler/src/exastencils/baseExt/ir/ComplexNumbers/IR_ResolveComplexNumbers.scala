package exastencils.baseExt.ir.ComplexNumbers

import exastencils.base.ir.IR_ComplexDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_VariableAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger

object IR_ResolveComplexNumbers extends DefaultStrategy("Resolve operations with complex expressions") {
  def isComplexExpr(expr : IR_Expression) : Boolean = expr.isInstanceOf[IR_ComplexExpression]
  def isComplexAcc(va : IR_Expression) : Boolean = va.datatype.isInstanceOf[IR_ComplexDatatype]
  def isComplexNumber(expr : IR_Expression) : Boolean = isComplexAcc(expr) || isComplexExpr(expr)
  def getAccName(expr : IR_Expression) : String = expr match {
    case va : IR_VariableAccess => va.name
    case _ => Logger.error("unexpected argument")
  }

  this += new Transformation("resolve ", {
        // handle imaginary and real parts as distinct nodes to have something similar as highDimAccesses for complex numbers
    case fcall : IR_FunctionCall if (fcall.name == "Im" && isComplexAcc(fcall.arguments(0))) =>
      IR_ComplexNumberAccessImag(getAccName(fcall.arguments(0)))
    case fcall : IR_FunctionCall if(fcall.name == "Re" && isComplexAcc(fcall.arguments(0)))                                        =>
      IR_ComplexNumberAccessReal(getAccName(fcall.arguments(0)))
    case fcall : IR_FunctionCall if(fcall.name == "equal" && (isComplexNumber(fcall.arguments(0)) || isComplexNumber(fcall.arguments(1)))) =>
      IR_ComplexNumberEqual(fcall.arguments(0), fcall.arguments(1))
    case fcall : IR_FunctionCall if(fcall.name == "notEqual" && (isComplexNumber(fcall.arguments(0)) || isComplexNumber(fcall.arguments(1)))) =>
      IR_ComplexNumberNotEqual(fcall.arguments(0), fcall.arguments(1))
  })
}
