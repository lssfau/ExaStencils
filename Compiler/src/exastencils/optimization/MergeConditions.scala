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

  this += new Transformation("now", new PartialFunction[Node, Transformation.OutputType] {
    override def isDefinedAt(node : Node) : Boolean = {
      node match {
        case cStmt : ConditionStatement =>
          val remove : Boolean = (collector.head eq parent) && (cStmt.condition == mergeInto.condition)
          if (!remove) {
            parent = collector.head
            mergeInto = cStmt
          }
          remove
        case _ =>
          parent = null
          mergeInto = null
          false
      }
    }
    override def apply(node : Node) : Transformation.OutputType = {
      return NullStatement
    }
  })
}
