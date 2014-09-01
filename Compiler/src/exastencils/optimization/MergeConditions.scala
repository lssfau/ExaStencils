package exastencils.optimization

import exastencils.core.StateManager
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.datastructures.ir._

object MergeConditions extends DefaultStrategy("Fuse Conditions") {

  private val collector = new StackCollector()
  private var parent : Node = null
  private var mergeInto : ConditionStatement = null

  override def apply(node : Option[Node] = None) : Unit = {
    StateManager.register(collector)
    super.apply(node)
    StateManager.unregister(collector)
  }

  this += new Transformation("now", {
    case cond : ConditionStatement =>
      if ((collector.head eq parent) && (cond.condition == mergeInto.condition)) {
        mergeInto.trueBody ++= cond.trueBody
        mergeInto.falseBody ++= cond.falseBody
        NullStatement
      } else {
        parent = collector.head
        mergeInto = cond
        cond
      }

    case n =>
      parent = null
      mergeInto = null
      n
  })
}
