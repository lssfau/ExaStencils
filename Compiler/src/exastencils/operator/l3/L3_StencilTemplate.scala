package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_Index
import exastencils.domain.l3.L3_Domain
import exastencils.knowledge.l3._
import exastencils.operator.l4.L4_StencilTemplate
import exastencils.prettyprinting.PpStream

/// L3_StencilTemplate

case class L3_StencilTemplate(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var localization : String, // localization of the stencil data
    var domain : L3_Domain, // domain the stencil lives on
    var offsets : ListBuffer[L3_Index]) extends L3_LeveledKnowledgeObject[L4_StencilTemplate] {

  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = ???
}
