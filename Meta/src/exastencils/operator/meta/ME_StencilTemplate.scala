package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_StencilTemplate extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_StencilTemplate.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.operator.|LAYER_LC|"""
    printer <<< """"""
    printer <<< """import scala.collection.mutable.ListBuffer"""
    printer <<< """"""
    printer <<< """import exastencils.base.|LAYER_LC|.|LAYER_UC|_Index"""
    printer <<< """import exastencils.domain.|LAYER_LC|.|LAYER_UC|_Domain"""
    printer <<< """import exastencils.knowledge.|LAYER_LC|._"""
    printer <<< """import exastencils.operator.|NEXT_LC|.|NEXT_UC|_StencilTemplate"""
    printer <<< """import exastencils.prettyprinting.PpStream"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_StencilTemplate"""
    printer <<< """"""
    printer <<< """case class |LAYER_UC|_StencilTemplate("""
    printer <<< """    var name : String, // will be used to find the stencil"""
    printer <<< """    var level : Int, // the level the stencil lives on"""
    printer <<< """    var localization : String, // localization of the stencil data"""
    printer <<< """    var domain : |LAYER_UC|_Domain, // domain the stencil lives on"""
    printer <<< """    var offsets : ListBuffer[|LAYER_UC|_Index]) extends |LAYER_UC|_LeveledKnowledgeObject[|NEXT_UC|_StencilTemplate] {"""
    printer <<< """"""
    printer <<< """  override def prettyprintDecl(out : PpStream) : Unit = ???"""
    if (L3 == layer) {
      printer <<< """  override def progressImpl() = ???"""
    }
    if (L2 == layer) {
      printer <<< """  override def progressImpl() = |NEXT_UC|_StencilTemplate(name, level, localization, domain.getProgressedObj(), offsets.map(_.progress))"""
    }
    printer <<< """}"""
    printer.toString
  }
}
