package exastencils.knowledge.l3

import exastencils.knowledge.l4.L4_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L3_KnowledgeObject

object L3_KnowledgeObject {
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[L3_KnowledgeObject[_]])
}

trait L3_KnowledgeObject[L4_Equivalent <: L4_KnowledgeObject[_]] {

  L3_KnowledgeObject

  def name : String
  def progressImpl() : L4_Equivalent
  def prettyprintDecl(out : PpStream) : Unit
  def createDuplicate() : L3_KnowledgeObject[L4_Equivalent]

  private var progressed : Option[L4_Equivalent] = None

  final def progress() : L4_Equivalent = {
    // progress if not already done - otherwise simply return the progressed object
    if (progressed.isEmpty)
      progressed = Some(progressImpl())
    progressed.get
  }

  // alias for progress -> progress object if not already progressed
  def getProgressedObj() : L4_Equivalent = progress()
}

/// L3_LeveledKnowledgeObject

trait L3_LeveledKnowledgeObject[L4_Equivalent <: L4_KnowledgeObject[_]] extends L3_KnowledgeObject[L4_Equivalent] {
  def level : Int
}
