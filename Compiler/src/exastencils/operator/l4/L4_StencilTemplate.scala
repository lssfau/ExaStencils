package exastencils.operator.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_Index
import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_StencilTemplate

object L4_StencilTemplate {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L4_StencilTemplate(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var offsets : ListBuffer[L4_Index]) extends L4_KnowledgeObjectWithLevel[IR_KnowledgeObject] {

  def prettyprintDecl(out : PpStream) = {
    out << "StencilTemplate " << name << "@" << level << " {\n"
    for (offset <- offsets)
      out << offset << " => " << "\n"
    out << "}"
  }

  override def progressImpl() = Logger.error("Trying to progress L4_StencilTemplate - unsupported")
}
