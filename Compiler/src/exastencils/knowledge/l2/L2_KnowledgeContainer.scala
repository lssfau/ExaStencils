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
      Logger.debug(s"Progressing ${ c.length } objects of collection ${ c.name }")
      c.progress()
    })
  }

  def clear() = collections.foreach(_.clear())
}
