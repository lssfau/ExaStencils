package exastencils.operator.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.knowledge.l2.L2_LeveledKnowledgeAccess
import exastencils.operator.l3.L3_OperatorAccess

/// L2_OperatorAccess

abstract class L2_OperatorAccess extends L2_LeveledKnowledgeAccess {
  override def progress : L3_OperatorAccess
  def assembleOffsetMap() : Map[L2_Expression, ListBuffer[L2_ConstIndex]]
}
