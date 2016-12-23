package exastencils.grid

import exastencils.grid.ir._

/// Grid

// helper object/method to branch grid types
object Grid {
  // shortcut functions
  def getGeometry = GridGeometry.getGeometry
  def getEvaluator = GridEvaluator.getEvaluator

  // strategies
  def applyStrategies() = {
    IR_PrepareShiftedEvaluationFunctions.apply() // workaround

    IR_ResolveEvalFunction.apply()
    IR_ResolveIntegrateFunction.apply()
    IR_ExpandEvalFunction.apply()

    IR_ResolveVirtualFieldAccess.apply()
  }
}
