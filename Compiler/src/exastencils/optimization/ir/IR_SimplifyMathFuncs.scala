package exastencils.optimization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_RealConstant
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.util.ir.IR_MathFunctionReference
import exastencils.util.ir.IR_MathFunctions

object IR_SimplifyMathFuncs extends DefaultStrategy("Evaluate math functions during code generation") {

  this += Transformation("Find evaluable functions", {
    // simplify math functions applied to constant fp values
    case fctCall @ IR_FunctionCall(fct : IR_MathFunctionReference, args) =>
      val constArgs : ListBuffer[Double] = ListBuffer()
      var isEvaluable = true
      args.foreach(arg => {
        try {
          val c = IR_SimplifyExpression.evalFloating(arg)
          constArgs += c
        } catch {
          case _ : EvaluationException =>
            isEvaluable = false
        }
      })

      if (isEvaluable)
        IR_RealConstant(IR_MathFunctions.evaluateMathFunction(fct.name, constArgs))
      else
        fctCall
  })
}
