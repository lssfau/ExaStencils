package exastencils.app.l4

import exastencils.baseExt.l4.L4_UnifyGlobalSections
import exastencils.scheduling.Scheduler
import exastencils.waLBerla.l4.field.L4_WaLBerlaFieldCollection
import exastencils.waLBerla.l4.field.L4_WaLBerlaFieldLayoutCollection
import exastencils.waLBerla.l4.interfacing.L4_UnifyWaLBerlaVarsSections
import exastencils.waLBerla.l4.refinement.L4_WaLBerlaRefinementSelectionCollection

object L4_WaLBerlaLayerHandler extends L4_LayerHandler {
  var scheduler : Scheduler = Scheduler()

  override def initialize() : Unit = {
    // additionally init waLBerla collections
    L4_WaLBerlaFieldCollection

    L4_WaLBerlaFieldLayoutCollection
    L4_WaLBerlaRefinementSelectionCollection

    L4_DefaultLayerHandler.initialize()
  }

  override def schedule() : Unit = {
    L4_DefaultLayerHandler.schedule()
    scheduler.queue ++= L4_DefaultLayerHandler.scheduler.queue

    /* extend schedule of default L4 layer handler */
    scheduler.appendToFirstFound(L4_UnifyGlobalSections, L4_UnifyWaLBerlaVarsSections)
  }

  override def print() : Unit = L4_DefaultLayerHandler.print()

  override def shutdown() : Unit = L4_DefaultLayerHandler.shutdown()
}
