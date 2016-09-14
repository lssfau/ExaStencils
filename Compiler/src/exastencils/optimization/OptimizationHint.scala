package exastencils.optimization

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_VariableAccess

trait OptimizationHint {
  var isParallel : Boolean = false
  var isVectorizable : Boolean = false
  val privateVars = new ListBuffer[IR_VariableAccess]()
  var isInnermost : Boolean = false
}
