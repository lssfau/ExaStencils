package exastencils.knowledge.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_KnowledgeObject extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/knowledge/|LAYER_LC|/|LAYER_UC|_KnowledgeObject.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.knowledge.|LAYER_LC|

import exastencils.knowledge.|NEXT_LC|.|NEXT_UC|_KnowledgeObject
import exastencils.prettyprinting.PpStream

/// |LAYER_UC|_KnowledgeObject

trait |LAYER_UC|_KnowledgeObject[|NEXT_UC|_Equivalent <: |NEXT_UC|_KnowledgeObject[_]] {

  exastencils.core.Duplicate.dontClone(this.getClass)

  def name : String
  def progressImpl() : |NEXT_UC|_Equivalent
  def prettyprintDecl(out : PpStream) : Unit

  private var progressed : Option[|NEXT_UC|_Equivalent] = None

  final def progress() : |NEXT_UC|_Equivalent = {
    // progress if not already done - otherwise simply return the progressed object
    if (progressed.isEmpty)
      progressed = Some(progressImpl())
    progressed.get
  }

  // alias for progress -> progress object if not already progressed
  def getProgressedObj() : |NEXT_UC|_Equivalent = progress()
}

/// |LAYER_UC|_LeveledKnowledgeObject

trait |LAYER_UC|_LeveledKnowledgeObject[|NEXT_UC|_Equivalent <: |NEXT_UC|_KnowledgeObject[_]] extends |LAYER_UC|_KnowledgeObject[|NEXT_UC|_Equivalent] {
  def level : Int
}
"""
  }
}
