package exastencils.knowledge.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_KnowledgeAccess extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/knowledge/|LAYER_LC|/|LAYER_UC|_KnowledgeAccess.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.knowledge.|LAYER_LC|

import exastencils.base.|LAYER_LC|.|LAYER_UC|_Access

/// |LAYER_UC|_KnowledgeAccess

trait |LAYER_UC|_KnowledgeAccess extends |LAYER_UC|_Access {
  def target : |LAYER_UC|_KnowledgeObject[_]
  override def name : String = target.name
}

/// |LAYER_UC|_LeveledKnowledgeAccess

trait |LAYER_UC|_LeveledKnowledgeAccess extends |LAYER_UC|_KnowledgeAccess {
  override def target : |LAYER_UC|_LeveledKnowledgeObject[_]
  override def name : String = target.name
  def level : Int = target.level
}
"""
  }
}
