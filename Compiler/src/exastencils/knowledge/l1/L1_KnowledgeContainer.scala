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

package exastencils.knowledge.l1

import scala.collection.mutable.ListBuffer

import exastencils.knowledge.l1.L1_KnowledgeContainer.L1_ProcessDeclarations
import exastencils.knowledge.l1.L1_KnowledgeContainer.L1_ResolveAccesses
import exastencils.logger.Logger
import exastencils.scheduling.NoStrategyWrapper
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

/// L1_ProcessDeclarationsAndResolveAccessesWrapper

object L1_ProcessDeclarationsAndResolveAccessesWrapper extends NoStrategyWrapper {

  def callback : () => Unit = () => {
    var matches = 0
    do {
      matches = 0
      matches += L1_ProcessDeclarations.applyAndCountMatches()
      matches += L1_ResolveAccesses.applyAndCountMatches()

    } while (matches > 0)
  }
}
