package exastencils.optimization

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_VariableAccess

@deprecated("to be integrated with loop nodes", "10.10.2016")
trait OptimizationHint {
  val privateVars = new ListBuffer[IR_VariableAccess]()
  var isInnermost : Boolean = false
}
