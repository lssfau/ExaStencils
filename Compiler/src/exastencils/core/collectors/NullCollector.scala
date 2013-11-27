package exastencils.core.collectors

import exastencils.datastructures._

class NullCollector extends Collector {
  def enter(node : Node) : Unit = {}
  def leave(node : Node) : Unit = {}
  def reset() : Unit = {}
}
