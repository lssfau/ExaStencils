package exastencils.fieldlike.ir

import exastencils.knowledge.ir.IR_LeveledKnowledgeAccess

/// IR_FieldLikeAccessLike

trait IR_FieldLikeAccessLike extends IR_LeveledKnowledgeAccess {
  def field : IR_FieldLike
  def target = field
}
