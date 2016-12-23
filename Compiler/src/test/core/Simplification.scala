package test.core

import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.config._
import exastencils.optimization.ir.IR_GeneralSimplify

object Simplification {
  def main(args : Array[String]) : Unit = {
    val index = IR_ExpressionIndex(1, 2, 3)
    val aabb = IR_ExpressionIndexRange(IR_ExpressionIndex(0, 0, 0), IR_ExpressionIndex(33, 33, 33))

    val node = Knowledge.dimensionality match {
      case 1 => index(0)
      case 2 => index(1) * (aabb.end(0) - aabb.begin(0)) + index(0)
      case 3 => index(2) * ((aabb.end(1) - aabb.begin(1)) * (aabb.end(0) - aabb.begin(0))) + index(1) * (aabb.end(0) - aabb.begin(0)) + index(0)
    }

    println(node)

    IR_GeneralSimplify.doUntilDoneStandalone(node)

    println(node)
  }
}
