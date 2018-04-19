package exastencils.knowledge.l2

import exastencils.knowledge.l3.L3_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L2_KnowledgeObject

object L2_KnowledgeObject {
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[L2_KnowledgeObject[_]])
}

trait L2_KnowledgeObject[L3_Equivalent <: L3_KnowledgeObject[_]] {

  L2_KnowledgeObject

  def name : String
  def progressImpl() : L3_Equivalent
  def prettyprintDecl(out : PpStream) : Unit
  def createDuplicate() : L2_KnowledgeObject[L3_Equivalent] = ???

  private var progressed : Option[L3_Equivalent] = None

  final def progress() : L3_Equivalent = {
    // progress if not already done - otherwise simply return the progressed object
    if (progressed.isEmpty)
      progressed = Some(progressImpl())
    progressed.get
  }

  // alias for progress -> progress object if not already progressed
  def getProgressedObj() : L3_Equivalent = progress()
}

/// L2_LeveledKnowledgeObject

trait L2_LeveledKnowledgeObject[L3_Equivalent <: L3_KnowledgeObject[_]] extends L2_KnowledgeObject[L3_Equivalent] {
  def level : Int
}
