package exastencils.stencil.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.boundary.l4.L4_NoBC
import exastencils.domain.l3.L3_Domain
import exastencils.field.l4.L4_Field
import exastencils.knowledge.l3.L3_KnowledgeObjectWithLevel
import exastencils.stencil.l4._

///// L3_StencilTemplate
//
//case class L3_StencilTemplate(
//    var name : String, // will be used to find the stencil
//    var level : Int, // the level the stencil lives on
//    var localization : String, // localization of the stencil data
//    var domain : L3_Domain, // domain the stencil lives on
//    var offsets : ListBuffer[L3_Index]) extends L3_KnowledgeObjectWithLevel {
//
//  def fieldLayoutName = s"defLayout${ offsets.length }$localization"
//  def resolveCoeffDataType = L3_RealDatatype // FIXME
//
//  def progress : (L4_StencilField, L4_Field, L4_StencilTemplate) = {
//    // split StencilTemplate into field, stencil template and stencil field
//    val field = L4_Field(name + "Data", level, 0 /* index to be set later */ , domain.getProgressedObject, fieldLayoutName, 1 /* only one slot */ , L4_NoBC /* no BC yet */)
//    val stencil = L4_StencilTemplate(name + "Template", level, localization, domain, offsets.map(_.progress))
//    val stencilField = L4_StencilField(name, level, field, stencil)
//    (stencilField, field, stencil)
//  }
//}
