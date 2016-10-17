package exastencils.baseExt.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

object IR_Linearization {
  def linearizeIndex(index : IR_Index, strides : IR_Index) : IR_Expression = {
    if (strides.length() != index.length()) Logger.warn(s"Index with dimensionality ${ index.length() } does not match strides with dimensionality ${ strides.length() }")

    val ret = (0 until math.min(strides.length(), index.length())).map(dim => {
      val stride = (0 until dim).map(strides.toExpressionIndex(_)).fold(1 : IR_Expression)(_ * _)
      index.toExpressionIndex(dim) * stride
    }).fold(0 : IR_Expression)(_ + _)

    IR_SimplifyExpression.simplifyIntegralExpr(ret)
  }
}
