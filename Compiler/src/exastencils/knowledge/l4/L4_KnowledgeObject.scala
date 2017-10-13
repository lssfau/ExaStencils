package exastencils.knowledge.l4

import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// L4_KnowledgeObject

trait L4_KnowledgeObject[IR_Equivalent <: IR_KnowledgeObject] {

  exastencils.core.Duplicate.dontClone(this.getClass)

  def name : String
  def progressImpl() : IR_Equivalent
  def prettyprintDecl(out : PpStream) : Unit

  private var progressed : Option[IR_Equivalent] = None

  final def progress() : IR_Equivalent = {
    // progress if not already done - otherwise simply return the progressed object
    if (progressed.isEmpty)
      progressed = Some(progressImpl())
    progressed.get
  }

  // alias for progress -> progress object if not already progressed
  def getProgressedObj() : IR_Equivalent = progress()
}

/// L4_LeveledKnowledgeObject

trait L4_LeveledKnowledgeObject[IR_Equivalent <: IR_KnowledgeObject] extends L4_KnowledgeObject[IR_Equivalent] {
  def level : Int
}
