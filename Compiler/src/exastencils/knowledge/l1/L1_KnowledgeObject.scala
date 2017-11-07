package exastencils.knowledge.l1

import exastencils.knowledge.l2.L2_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L1_KnowledgeObject

trait L1_KnowledgeObject[L2_Equivalent <: L2_KnowledgeObject[_]] {

  exastencils.core.Duplicate.dontClone(this.getClass)

  def name : String
  def progressImpl() : L2_Equivalent
  def prettyprintDecl(out : PpStream) : Unit

  private var progressed : Option[L2_Equivalent] = None

  final def progress() : L2_Equivalent = {
    // progress if not already done - otherwise simply return the progressed object
    if (progressed.isEmpty)
      progressed = Some(progressImpl())
    progressed.get
  }

  // alias for progress -> progress object if not already progressed
  def getProgressedObj() : L2_Equivalent = progress()

  def overwriteProgressed(newObj : L2_Equivalent) = { progressed = Some(newObj) }
}

/// L1_LeveledKnowledgeObject

trait L1_LeveledKnowledgeObject[L2_Equivalent <: L2_KnowledgeObject[_]] extends L1_KnowledgeObject[L2_Equivalent] {
  def level : Int
}
