package exastencils.util

import scala.collection.mutable.HashMap

import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.logger._

class CountingStrategy(id : String) extends DefaultStrategy("Counting " + id) {
  var nodes = 0
  var annotations = 0
  this += Transformation("really count", {
    case x =>
      nodes += 1
      annotations += x.annotations.size
      x
  })

  override def apply(node : Option[Node]) = {
    nodes = 0
    super.apply(node)
    Logger.dbg(s"Counting $id resulted in $nodes nodes with $annotations annotations")
  }
}

object CollectTimers extends DefaultStrategy("Collecting used timers") {
  var timers : HashMap[String, iv.Timer] = HashMap()

  override def apply(node : Option[Node] = None) = {
    timers.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    timers.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collecting", {
    case timer : iv.Timer => // TODO: don't overwrite for performance reasons
      timers += (timer.resolveName -> timer)
      timer
  })
}
