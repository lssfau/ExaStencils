package exastencils.util

import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.logger._

class CountingStrategy(id : String) extends DefaultStrategy("Counting " + id) {
  var nodes = 0
  this += Transformation("really count", { case x => nodes += 1; x })

  override def apply(node : Option[Node]) = {
    nodes = 0
    super.apply(node)
    Logger.dbg("Counting " + id + " resulted in " + nodes + " nodes")
  }
}
