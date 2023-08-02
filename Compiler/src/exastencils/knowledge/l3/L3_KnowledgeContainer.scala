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

package exastencils.knowledge.l3

import scala.collection.mutable.ListBuffer

import exastencils.config.Knowledge
import exastencils.grid.l3.L3_ResolveEvaluateOnGrid
import exastencils.grid.l3.L3_ResolveIntegrateOnGrid
import exastencils.knowledge.l3.L3_KnowledgeContainer.L3_ProcessDeclarations
import exastencils.knowledge.l3.L3_KnowledgeContainer.L3_ResolveAccesses
import exastencils.logger.Logger
import exastencils.scheduling.NoStrategyWrapper
import exastencils.solver.l3.L3_ProcessSolverForEquations
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
      Logger.debug(s"Progressing ${ c.length } objects of collection ${ c.name }")
      c.progress()
    })
  }

  def clear() = collections.foreach(_.clear())
}

/// L3_ProcessDeclarationsAndResolveAccessesWrapper

object L3_ProcessDeclarationsAndResolveAccessesWrapper extends NoStrategyWrapper {

  def callback : () => Unit = () => {
    var matches = 0
    do {
      matches = 0
      matches += L3_ProcessDeclarations.applyAndCountMatches()

      L3_ProcessSolverForEquations.apply()
      matches += (if (L3_ProcessSolverForEquations.results.isEmpty) 0 else L3_ProcessSolverForEquations.results.last._2.matches)

      matches += L3_ResolveAccesses.applyAndCountMatches()

      if (Knowledge.experimental_l3_resolveVirtualFields) {
        // integrate before evaluate -> might be nested
        L3_ResolveIntegrateOnGrid.apply()
        matches += (if (L3_ResolveIntegrateOnGrid.results.isEmpty) 0 else L3_ResolveIntegrateOnGrid.results.last._2.matches)

        L3_ResolveEvaluateOnGrid.apply()
        matches += (if (L3_ResolveEvaluateOnGrid.results.isEmpty) 0 else L3_ResolveEvaluateOnGrid.results.last._2.matches)
      }
    } while (matches > 0)
  }
}
