package exastencils.timing.ir

import scala.collection.mutable.HashMap

import exastencils.datastructures._

/// IR_CollectTimers

object IR_CollectTimers extends DefaultStrategy("Collect all timers used") {
  var timers : HashMap[String, IR_IV_Timer] = HashMap()

  override def apply(node : Option[Node] = None) = {
    timers.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    timers.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collect", {
    case timer : IR_IV_Timer =>
      timers += (timer.resolveName -> timer)
      timer
  })
}
