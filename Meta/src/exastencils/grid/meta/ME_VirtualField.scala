package exastencils.grid.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_VirtualField extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/grid/|LAYER_LC|/|LAYER_UC|_VirtualField.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.grid.|LAYER_LC|

import exastencils.base.|LAYER_LC|._
import exastencils.domain.|LAYER_LC|.|LAYER_UC|_Domain
import exastencils.grid.|NEXT_LC|.|NEXT_UC|_VirtualField
import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream

/// |LAYER_UC|_VirtualField

case class |LAYER_UC|_VirtualField(
    var name : String,
    var level : Int,
    var domain : |LAYER_UC|_Domain,
    var datatype : |LAYER_UC|_Datatype,
    var localization : String) extends |LAYER_UC|_LeveledKnowledgeObject[|NEXT_UC|_VirtualField] {

  override def prettyprintDecl(out : PpStream) : Unit = ???
  override def progressImpl() = |NEXT_UC|_VirtualField(name, level, domain.getProgressedObj(), datatype.progress, localization)
}
"""
  }
}
