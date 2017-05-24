package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_Index
import exastencils.domain.l2.L2_Domain
import exastencils.knowledge.l2._
import exastencils.operator.l3.L3_StencilTemplate
import exastencils.prettyprinting.PpStream

/// L2_StencilTemplate

case class L2_StencilTemplate(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var localization : String, // localization of the stencil data
    var domain : L2_Domain, // domain the stencil lives on
    var offsets : ListBuffer[L2_Index]) extends L2_LeveledKnowledgeObject[L3_StencilTemplate] {

  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = L3_StencilTemplate(name, level, localization, domain.getProgressedObj(), offsets.map(_.progress))
}
