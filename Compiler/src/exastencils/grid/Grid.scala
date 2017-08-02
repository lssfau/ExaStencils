package exastencils.grid

import exastencils.datastructures.Node
import exastencils.grid.ir._

/// Grid

// helper object/method to branch grid types
object Grid {
  // shortcut functions
  def getGeometry = GridGeometry.getGeometry
  def getEvaluator = GridEvaluator.getEvaluator

  // strategies
  def applyStrategies(node : Option[Node] = None) = {
    IR_PrepareShiftedEvaluationFunctions.apply(node) // workaround

    IR_ResolveEvaluateOnGrid.apply(node)
    IR_ResolveIntegrateOnGrid.apply(node)
    IR_ExpandEvaluationExpressions.apply(node)

    IR_ResolveVirtualFieldAccesses.apply(node)
  }
}
