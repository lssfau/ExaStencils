package exastencils.optimization

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir.VariableAccess

trait OptimizationHint {
  var isParallel : Boolean = false
  val privateVars = new ListBuffer[VariableAccess]()
  var isInnermost : Boolean = false
}
