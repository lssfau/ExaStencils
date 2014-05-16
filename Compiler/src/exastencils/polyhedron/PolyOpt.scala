package exastencils.polyhedron

import exastencils.core.Logger
import exastencils.core.StateManager
import exastencils.datastructures.Strategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.convFromNode
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.primitives.LoopOverDimensions

object PolyOpt extends Strategy("Polyhedral optimizations") {

  def apply() : Unit = {
    StateManager.register(Extractor)
    this += new Transformation("extract model", PartialFunction.empty)
    super.apply()
    StateManager.unregister(Extractor)

    Logger.debug("    SCoPs: " + Extractor.scops.size)
    Logger.debug("    trash: " + Extractor.trash.size)
  }
}
