package exastencils.operator.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.knowledge.l3.L3_LeveledKnowledgeAccess
import exastencils.operator.l4.L4_OperatorAccess

/// L3_OperatorAccess

abstract class L3_OperatorAccess extends L3_LeveledKnowledgeAccess {
  override def progress : L4_OperatorAccess
  def assembleOffsetMap() : Map[L3_Expression, ListBuffer[L3_ConstIndex]]
}
