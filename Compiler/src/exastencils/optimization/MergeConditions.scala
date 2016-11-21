package exastencils.optimization

import exastencils.base.ir._
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._

object MergeConditions extends DefaultStrategy("Fuse Conditions") {

  private val collector = new StackCollector()
  private var parent : Node = null
  private var mergeInto : IR_IfCondition = null
  this.register(collector)

  this += new Transformation("now", new PartialFunction[Node, Transformation.OutputType] {
    override def isDefinedAt(node : Node) : Boolean = {
      node match {
        case cStmt : IR_IfCondition =>
          val remove : Boolean = (collector.head eq parent) && (cStmt.condition == mergeInto.condition)
          if (!remove) {
            parent = collector.head
            mergeInto = cStmt
          }
          remove
        case _                      =>
          parent = null
          mergeInto = null
          false
      }
    }
    override def apply(node : Node) : Transformation.OutputType = {
      val cond = node.asInstanceOf[IR_IfCondition]
      mergeInto.trueBody ++= cond.trueBody
      mergeInto.falseBody ++= cond.falseBody
      IR_NullStatement
    }
  })
}
