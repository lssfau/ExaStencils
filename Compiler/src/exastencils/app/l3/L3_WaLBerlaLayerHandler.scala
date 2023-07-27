package exastencils.app.l3

import exastencils.scheduling.Scheduler
import exastencils.waLBerla.l3.field.L3_WaLBerlaFieldCollection

object L3_WaLBerlaLayerHandler extends L3_LayerHandler {

  override def scheduler : Scheduler = Scheduler()

  override def initialize() : Unit = {
    // additionally init waLBerla collections
    L3_WaLBerlaFieldCollection

    L3_DefaultLayerHandler.initialize()
  }

  override def schedule() : Unit = {
    L3_DefaultLayerHandler.schedule()
    scheduler.queue ++= L3_DefaultLayerHandler.scheduler.queue

    /* extend schedule of default L3 layer handler */
  }

  override def print() : Unit = L3_DefaultLayerHandler.print()

  override def shutdown() : Unit = L3_DefaultLayerHandler.shutdown()
}

