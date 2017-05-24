package exastencils.knowledge.l3

import scala.collection.mutable.ListBuffer

import exastencils.logger.Logger
import exastencils.util.StrategyContainer

/// L3_KnowledgeContainer

object L3_KnowledgeContainer {

  val collections = ListBuffer[L3_KnowledgeCollection]()

  object L3_PrepareDeclarations extends StrategyContainer

  object L3_PrepareAccesses extends StrategyContainer

  object L3_ProcessDeclarations extends StrategyContainer

  object L3_ResolveAccesses extends StrategyContainer

  def register(collection : L3_KnowledgeCollection) = collections += collection
  def progress() = {
    collections.foreach(c => {
      Logger.warn(s"Progressing ${ c.length } objects of collection ${ c.name }")
      c.progress()
    })
  }
}
