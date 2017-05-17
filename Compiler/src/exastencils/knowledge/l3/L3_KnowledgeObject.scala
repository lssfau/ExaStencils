package exastencils.knowledge.l3

import exastencils.knowledge.l4.L4_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_KnowledgeObject

trait L3_KnowledgeObject[L4_Equivalent <: L4_KnowledgeObject[_]] {

  exastencils.core.Duplicate.dontClone(this.getClass)

  def name : String
  def progressImpl() : L4_Equivalent
  def prettyprintDecl(out : PpStream) : Unit

  final def progress() : L4_Equivalent = {
    progressed = Some(progressImpl())
    progressed.get
  }

  var progressed : Option[L4_Equivalent] = None
  def getProgressedObject() : L4_Equivalent = {
    if (progressed.isEmpty)
      Logger.warn(s"Trying to access invalid progressed object of type ${ this.getClass.getName } with name ${ name }")
    progressed.get
  }
}

/// L3_LeveledKnowledgeObject

trait L3_LeveledKnowledgeObject[L4_Equivalent <: L4_KnowledgeObject[_]] extends L3_KnowledgeObject[L4_Equivalent] {
  def level : Int
}
