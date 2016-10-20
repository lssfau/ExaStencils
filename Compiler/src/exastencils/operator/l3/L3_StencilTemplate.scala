package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain
import exastencils.knowledge.l3.L3_KnowledgeObjectWithLevel
import exastencils.operator.l4.L4_StencilTemplate
import exastencils.prettyprinting.PpStream

/// L3_StencilTemplate

object L3_StencilTemplate {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L3_StencilTemplate(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var localization : String, // localization of the stencil data
    var domain : L3_Domain, // domain the stencil lives on
    var offsets : ListBuffer[L3_Index]) extends L3_KnowledgeObjectWithLevel[L4_StencilTemplate] {

  def fieldLayoutName = s"defLayout${ offsets.length }$localization"
  def resolveCoeffDataType = L3_RealDatatype // FIXME

  //  def progressField = {
  //    val layout = L4_FieldLayoutCollection.getByIdentifier(fieldLayoutName, level).get
  //    L4_Field(name + "Data", level, 0 /* index to be set later */ , domain.getProgressedObject().name, layout, 1 /* only one slot */ , L4_NoBC /* no BC yet */)
  //  }
  //
  //  def progressStencilField = L4_StencilField(name, level, field, stencil)

  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = L4_StencilTemplate(name + "Template", level, localization, domain.getProgressedObject(), offsets.map(_.progress))
}
