package exastencils.performance.ir

import exastencils.util.ir.IR_CollectFunctionStatements

/// IR_PerformanceEstimate

case class IR_PerformanceEstimate(var host : Double, var device : Double) {
  // TODO: integrate host/device choices to estimate final execution time
  def +(other : IR_PerformanceEstimate) = IR_PerformanceEstimate(host + other.host, device + other.device)
}

/// IR_AddPerformanceEstimates

object IR_AddPerformanceEstimates {
  def apply() = {
    IR_CollectFunctionStatements.apply()
    IR_EvaluatePerformanceEstimates.doUntilDone()
  }
}
