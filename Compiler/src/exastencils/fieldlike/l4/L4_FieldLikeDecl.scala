package exastencils.fieldlike.l4

import scala.reflect.runtime.universe._
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl

abstract class L4_FieldLikeDecl[L4_Type <: L4_FieldLike[IR_Type, _] : TypeTag, IR_Type <: IR_FieldLike] extends L4_LeveledKnowledgeDecl {
  def addToKnowledge() : Unit

  def associatedCollection : L4_FieldLikeCollection[L4_Type, IR_Type]
}
