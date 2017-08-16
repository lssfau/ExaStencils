package exastencils.grid

import exastencils.datastructures.Node
import exastencils.grid.ir._

/// Grid

// helper object/method to branch grid types
object Grid {
  // shortcut functions
  def getGeometry = GridGeometry.getGeometry

  // strategies
  def applyStrategies(node : Option[Node] = None) = {
    IR_ResolveIntegrateOnGrid.apply()
    IR_ResolveEvaluateOnGrid.apply()

    IR_ResolveVirtualFieldAccesses.apply(node)
  }
}
