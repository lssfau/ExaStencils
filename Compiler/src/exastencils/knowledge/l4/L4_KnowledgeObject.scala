package exastencils.knowledge.l4

import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_KnowledgeObject

trait L4_KnowledgeObject[IR_Equivalent <: IR_KnowledgeObject] {
  def name : String
  def progressImpl() : IR_Equivalent
  def prettyprintDecl(out : PpStream) : Unit

  final def progress() : IR_Equivalent = {
    progressed = Some(progressImpl)
    progressed.get
  }

  var progressed : Option[IR_Equivalent] = None
  def getProgressedObject() : IR_Equivalent = {
    if (progressed.isEmpty)
      Logger.warn(s"Trying to access invalid progressed object of type ${ this.getClass.getName } with name ${ name }")
    progressed.get
  }
}

/// L4_KnowledgeObjectWithLevel

trait L4_KnowledgeObjectWithLevel[IR_Equivalent <: IR_KnowledgeObject] extends L4_KnowledgeObject[IR_Equivalent] {
  def level : Int
}
