package exastencils.optimization.ir

import scala.collection.mutable

import exastencils.base.ir._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.field.ir.IR_LinearizedFieldAccess

object IR_SimplifyIndexExpressions extends DefaultStrategy("Simplify index expressions") {
  val loopExtremaCollector = new IR_LoopExtremaCollector()
  this.onBefore = () => loopExtremaCollector.reset()

  this.register(loopExtremaCollector)

  this += new Transformation("now", {
    case a : IR_ArrayAccess =>
      a.index = IR_SimplifyExpression.simplifyIntegralExpr(a.index, loopExtremaCollector.extremaMap)
      a

    case d : IR_LinearizedFieldAccess =>
      d.index = IR_SimplifyExpression.simplifyIntegralExpr(d.index, loopExtremaCollector.extremaMap)
      d
  })
}
