package exastencils.operator.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_Index
import exastencils.domain.l4.L4_Domain
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel
import exastencils.operator.ir.IR_StencilTemplate
import exastencils.prettyprinting._

/// L4_StencilTemplate

object L4_StencilTemplate {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L4_StencilTemplate(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var localization : String, // localization of the stencil data
    var domain : L4_Domain, // domain the stencil lives on
    var offsets : ListBuffer[L4_Index]) extends L4_KnowledgeObjectWithLevel[IR_StencilTemplate] {

  def prettyprintDecl(out : PpStream) = {
    out << "Stencil " << name << "@(" << level << ") {\n"
    for (offset <- offsets)
      out << offset << " => " << 0 << "\n"
    out << "\n}\n"
  }

  override def progressImpl() = IR_StencilTemplate(name, level, domain.getProgressedObject(), offsets.map(_.progress))
}
