package exastencils.operator.ir

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.knowledge.ir.IR_LeveledKnowledgeAccess

/// IR_OperatorAccess

abstract class IR_OperatorAccess extends IR_LeveledKnowledgeAccess {
  def assembleOffsetMap() : Map[IR_Expression, ListBuffer[IR_ConstIndex]]
  def stencil : IR_Stencil
}
