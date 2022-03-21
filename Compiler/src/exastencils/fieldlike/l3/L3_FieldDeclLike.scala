package exastencils.fieldlike.l3

import exastencils.knowledge.l3.L3_LeveledKnowledgeDecl

abstract class L3_FieldDeclLike[L3_Type <: L3_FieldLike[_]] extends L3_LeveledKnowledgeDecl {
  def addToKnowledge() : Unit
}
