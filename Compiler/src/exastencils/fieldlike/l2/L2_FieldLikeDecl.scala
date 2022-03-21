package exastencils.fieldlike.l2

import exastencils.knowledge.l2.L2_LeveledKnowledgeDecl

abstract class L2_FieldLikeDecl[L2_Type <: L2_FieldLike[_]] extends L2_LeveledKnowledgeDecl{
  def addToKnowledge() : Unit
}
