package exastencils.knowledge.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_KnowledgeContainer extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/knowledge/|LAYER_LC|/|LAYER_UC|_KnowledgeContainer.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.knowledge.|LAYER_LC|

import scala.collection.mutable.ListBuffer

import exastencils.logger.Logger
import exastencils.util.StrategyContainer

/// |LAYER_UC|_KnowledgeContainer

object |LAYER_UC|_KnowledgeContainer {

  val collections = ListBuffer[|LAYER_UC|_KnowledgeCollection]()

  object |LAYER_UC|_PrepareDeclarations extends StrategyContainer

  object |LAYER_UC|_PrepareAccesses extends StrategyContainer

  object |LAYER_UC|_ProcessDeclarations extends StrategyContainer

  object |LAYER_UC|_ResolveAccesses extends StrategyContainer

  def register(collection : |LAYER_UC|_KnowledgeCollection) = collections += collection
  def progress() = {
    collections.foreach(c => {
      Logger.warn(s"Progressing ${ c.length } objects of collection ${ c.name }")
      c.progress()
    })
  }
}
"""
  }
}
