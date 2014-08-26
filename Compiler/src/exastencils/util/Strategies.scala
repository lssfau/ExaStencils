package exastencils.util

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._

class CountingStrategy(id : String) extends DefaultStrategy("Counting " + id) {
  var nodes = 0
  this += Transformation("really count", { case x => nodes += 1; x })

  override def apply(node : Option[Node]) = {
    nodes = 0
    super.apply(node)
    Logger.warn("Counting " + id + " resulted in " + nodes + " nodes")
  }
}
