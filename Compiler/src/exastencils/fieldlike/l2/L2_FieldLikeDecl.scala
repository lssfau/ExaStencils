package exastencils.fieldlike.l2

import exastencils.fieldlike.l3.L3_FieldLike
import exastencils.knowledge.l2.L2_LeveledKnowledgeDecl

abstract class L2_FieldLikeDecl[L2_Type <: L2_FieldLike[L3_Type], L3_Type <: L3_FieldLike[_]] extends L2_LeveledKnowledgeDecl{
  def addToKnowledge() : Unit

  def associatedCollection : L2_FieldLikeCollection[L2_Type, L3_Type]
}
