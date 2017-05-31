package exastencils.knowledge.l4

import scala.collection.mutable.ListBuffer

import exastencils.logger.Logger
import exastencils.util.StrategyContainer

/// L4_KnowledgeContainer

object L4_KnowledgeContainer {

  val collections = ListBuffer[L4_KnowledgeCollection]()

  object L4_PrepareDeclarations extends StrategyContainer

  object L4_PrepareAccesses extends StrategyContainer

  object L4_ProcessDeclarations extends StrategyContainer

  object L4_ResolveAccesses extends StrategyContainer

  def register(collection : L4_KnowledgeCollection) = collections += collection
  def progress() = {
    collections.foreach(c => {
      Logger.warn(s"Progressing ${ c.length } objects of collection ${ c.name }")
      c.progress()
    })
  }

  def clear() = collections.foreach(_.clear())
}
