package exastencils.app.l2

import exastencils.scheduling.Scheduler
import exastencils.waLBerla.l2.field.L2_WaLBerlaFieldCollection

object L2_WaLBerlaLayerHandler extends L2_LayerHandler {

  override def scheduler : Scheduler = Scheduler()

  override def initialize() : Unit = {
    // additionally init waLBerla collections
    L2_WaLBerlaFieldCollection

    L2_DefaultLayerHandler.initialize()
  }

  override def schedule() : Unit = {
    L2_DefaultLayerHandler.schedule()
    scheduler.queue ++= L2_DefaultLayerHandler.scheduler.queue

    /* extend schedule of default L2 layer handler */
  }

  override def print() : Unit = L2_DefaultLayerHandler.print()

  override def shutdown() : Unit = L2_DefaultLayerHandler.shutdown()
}

