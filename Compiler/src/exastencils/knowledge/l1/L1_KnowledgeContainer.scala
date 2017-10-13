package exastencils.knowledge.l1

import scala.collection.mutable.ListBuffer

import exastencils.logger.Logger
import exastencils.util.StrategyContainer

/// L1_KnowledgeContainer

object L1_KnowledgeContainer {

  val collections = ListBuffer[L1_KnowledgeCollection]()

  object L1_PrepareDeclarations extends StrategyContainer

  object L1_PrepareAccesses extends StrategyContainer

  object L1_ProcessDeclarations extends StrategyContainer

  object L1_ResolveAccesses extends StrategyContainer

  def register(collection : L1_KnowledgeCollection) = collections += collection
  def progress() = {
    collections.foreach(c => {
      Logger.debug(s"Progressing ${ c.length } objects of collection ${ c.name }")
      c.progress()
    })
  }

  def clear() = collections.foreach(_.clear())
}
