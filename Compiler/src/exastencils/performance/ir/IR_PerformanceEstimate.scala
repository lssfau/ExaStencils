//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.performance.ir

import exastencils.config.Knowledge
import exastencils.scheduling.NoStrategyWrapper
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

/// IR_AddPerformanceEstimatesWrapper

object IR_AddPerformanceEstimatesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    if (Knowledge.performance_addEstimation)
      IR_AddPerformanceEstimates.apply()
  }
}
