package exastencils.knowledge.l2

import scala.collection.mutable.ListBuffer

import exastencils.logger.Logger
import exastencils.util.StrategyContainer

/// L2_KnowledgeContainer

object L2_KnowledgeContainer {

  val collections = ListBuffer[L2_KnowledgeCollection]()

  object L2_PrepareDeclarations extends StrategyContainer

  object L2_PrepareAccesses extends StrategyContainer

  object L2_ProcessDeclarations extends StrategyContainer

  object L2_ResolveAccesses extends StrategyContainer

  def register(collection : L2_KnowledgeCollection) = collections += collection
  def progress() = {
    collections.foreach(c => {
      Logger.warn(s"Progressing ${ c.length } objects of collection ${ c.name }")
      c.progress()
    })
  }
}
