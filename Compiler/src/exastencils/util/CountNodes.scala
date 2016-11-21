package exastencils.util

import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.logger._

class CountNodes(id : String) extends DefaultStrategy("Count " + id) {
  var nodes = 0
  var annotations = 0
  this += Transformation("Count", {
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
