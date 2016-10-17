package exastencils.stencil.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_Index
import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.knowledge.l4.L4_KnowledgeObjectWithIdentAndLevel
import exastencils.prettyprinting._

/// L4_StencilTemplate

object L4_StencilTemplate {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L4_StencilTemplate(
    var identifier : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var localization : String, // localization of the stencil data
    var domain : String, // domain the stencil lives on
    var offsets : ListBuffer[L4_Index]) extends L4_KnowledgeObjectWithIdentAndLevel {

  def prettyprintDecl(out : PpStream) = {
    out << "Stencil " << identifier << "@(" << level << ") {\n"
    for (offset <- offsets)
      out << offset << " => " << 0 << "\n"
    out << "\n}\n"
  }

  override def progress : IR_KnowledgeObject = ???

  override def getProgressedObject : IR_KnowledgeObject = ???
}
