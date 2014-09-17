package exastencils.core.collectors

import exastencils.datastructures._

class NullCollector extends Collector {
  override def enter(node : Node) : Unit = {}
  override def leave(node : Node) : Unit = {}
  override def reset() : Unit = {}
}
