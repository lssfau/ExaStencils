package exastencils.fieldlike.ir

import scala.collection.mutable.ListBuffer

import exastencils.knowledge.ir.IR_LeveledKnowledgeCollection

object IR_FieldLikeLayoutCollections {
  val collections = ListBuffer[IR_FieldLikeLayoutCollection]()

  def register(collection : IR_FieldLikeLayoutCollection) =
    collections += collection

  def clear() = collections.foreach(_.clear())
}

class IR_FieldLikeLayoutCollection extends IR_LeveledKnowledgeCollection[IR_FieldLikeLayout]