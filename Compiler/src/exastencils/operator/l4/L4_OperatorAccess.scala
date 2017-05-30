package exastencils.operator.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.knowledge.l4.L4_LeveledKnowledgeAccess
import exastencils.operator.ir.IR_OperatorAccess

/// L4_OperatorAccess

abstract class L4_OperatorAccess extends L4_LeveledKnowledgeAccess {
  override def progress : IR_OperatorAccess
  def assembleOffsetMap() : Map[L4_Expression, ListBuffer[L4_ConstIndex]]
}
