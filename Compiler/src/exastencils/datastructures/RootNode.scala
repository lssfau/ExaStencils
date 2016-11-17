package exastencils.datastructures

import scala.collection.mutable.ListBuffer

trait RootNode extends exastencils.datastructures.Node {
  def nodes : ListBuffer[Node]
}
