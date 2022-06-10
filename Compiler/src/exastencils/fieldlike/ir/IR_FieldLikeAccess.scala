package exastencils.fieldlike.ir

import exastencils.knowledge.ir.IR_LeveledKnowledgeAccess

/// IR_FieldLikeAccess

trait IR_FieldLikeAccess extends IR_LeveledKnowledgeAccess {
  def field : IR_FieldLike
  def target = field
}
