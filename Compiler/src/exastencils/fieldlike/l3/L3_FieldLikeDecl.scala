package exastencils.fieldlike.l3

import exastencils.fieldlike.l4.L4_FieldLike
import exastencils.knowledge.l3.L3_LeveledKnowledgeDecl

abstract class L3_FieldLikeDecl[L3_Type <: L3_FieldLike[L4_Type], L4_Type <: L4_FieldLike[_, _]] extends L3_LeveledKnowledgeDecl {
  def addToKnowledge() : Unit

  def associatedCollection : L3_FieldLikeCollection[L3_Type, L4_Type]
}
