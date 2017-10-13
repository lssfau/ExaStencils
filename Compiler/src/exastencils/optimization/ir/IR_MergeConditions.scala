package exastencils.optimization.ir

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.util.ir.IR_StackCollector

object IR_MergeConditions extends DefaultStrategy("Fuse Conditions") {

  private val collector = new IR_StackCollector()
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
