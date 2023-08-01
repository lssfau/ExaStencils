//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.knowledge.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.config.Knowledge
import exastencils.grid.l4.L4_ResolveEvaluateOnGrid
import exastencils.grid.l4.L4_ResolveIntegrateOnGrid
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ProcessDeclarations
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ResolveAccesses
import exastencils.logger.Logger
import exastencils.scheduling.NoStrategyWrapper
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
      Logger.debug(s"Progressing ${ c.length } objects of collection ${ c.name }")
      c.progress()
    })
  }

  def clear() = collections.foreach(_.clear())
}

/// L4_ProcessDeclarationsAndResolveAccessesWrapper

object L4_ProcessDeclarationsAndResolveAccessesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    var matches = 0
    do {
      matches = 0
      matches += L4_ProcessDeclarations.applyAndCountMatches()
      matches += L4_ResolveAccesses.applyAndCountMatches()

      if (Knowledge.experimental_l4_resolveVirtualFields) {
        // integrate before evaluate -> might be nested
        L4_ResolveIntegrateOnGrid.apply()
        matches += (if (L4_ResolveIntegrateOnGrid.results.isEmpty) 0 else L4_ResolveIntegrateOnGrid.results.last._2.matches)

        L4_ResolveEvaluateOnGrid.apply()
        matches += (if (L4_ResolveEvaluateOnGrid.results.isEmpty) 0 else L4_ResolveEvaluateOnGrid.results.last._2.matches)
      }
    } while (matches > 0)

    if (ExaRootNode.l4_root.nodes.exists(_.isInstanceOf[L4_KnowledgeDecl])) {
      val filtered = ExaRootNode.l4_root.nodes.filter(_.isInstanceOf[L4_KnowledgeDecl])
      Logger.warn(s"L4 root has ${ filtered.length } unprocessed declaration nodes remaining:")
      filtered.foreach(Logger.warn(_))
    }
  }
}
