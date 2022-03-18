package exastencils.fieldlike.l4

import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl

abstract class L4_FieldDeclLike[L4_Type <: L4_FieldLike[_, _]] extends L4_LeveledKnowledgeDecl {
  def addToKnowledge() : Unit
}
