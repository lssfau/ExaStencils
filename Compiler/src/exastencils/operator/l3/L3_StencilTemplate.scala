package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.boundary.l4.L4_NoBC
import exastencils.domain.l3.L3_Domain
import exastencils.field.l4._
import exastencils.knowledge.l3.L3_KnowledgeObjectWithLevel
import exastencils.operator.l4.L4_StencilTemplate
import exastencils.prettyprinting.PpStream
import exastencils.stencil.l4.L4_StencilField

/// L3_StencilTemplate

case class L3_StencilTemplate(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var localization : String, // localization of the stencil data
    var domain : L3_Domain, // domain the stencil lives on
    var offsets : ListBuffer[L3_Index]) extends L3_KnowledgeObjectWithLevel[L4_StencilTemplate] {

  def fieldLayoutName = s"defLayout${ offsets.length }$localization"
  def resolveCoeffDataType = L3_RealDatatype // FIXME

  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = L4_StencilTemplate(name + "Template", level, offsets.map(_.progress))

  def progressField() : L4_Field = {
    val layout = L4_FieldLayoutCollection.getByIdentifier(fieldLayoutName, level).get
    L4_Field(name + "Data", level, 0 /* index to be set later */ , domain.getProgressedObject().name, layout, 1 /* only one slot */ , L4_NoBC /* no BC yet */)
  }

  def progressStencilField(field : L4_Field) = L4_StencilField(name, level, field, name + "Template", offsets.map(_.progress))
}
