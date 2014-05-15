package exastencils.polyhedron

import exastencils.core.ERROR
import exastencils.core.DBG
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

    DBG("    SCoPs: " + Extractor.scops.size)
    DBG("    trash: " + Extractor.trash.size)
  }
}
