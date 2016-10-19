package exastencils.knowledge.l2

import exastencils.knowledge.l3.L3_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_KnowledgeObject

trait L2_KnowledgeObject[L3_Equivalent <: L3_KnowledgeObject[_]] {
  def name : String
  def progressImpl() : L3_Equivalent
  def prettyprintDecl(out : PpStream) : Unit

  final def progress() : L3_Equivalent = {
    progressed = Some(progressImpl)
    progressed.get
  }

  var progressed : Option[L3_Equivalent] = None
  def getProgressedObject() : L3_Equivalent = {
    if (progressed.isEmpty)
      Logger.warn(s"Trying to access invalid progressed object of type ${ this.getClass.getName } with name ${ name }")
    progressed.get
  }
}

/// L2_KnowledgeObjectWithLevel

trait L2_KnowledgeObjectWithLevel[L3_Equivalent <: L3_KnowledgeObject[_]] extends L2_KnowledgeObject[L3_Equivalent] {
  def level : Int
}
