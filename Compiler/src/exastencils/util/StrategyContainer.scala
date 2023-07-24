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

package exastencils.util

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.scheduling.Schedulable

/// StrategyContainer

abstract class StrategyContainer extends Schedulable {
  var strategies = ListBuffer[DefaultStrategy]()

  def apply() = strategies.foreach(_.apply())

  override def apply(applyAtNode : Option[Node]) : Unit = apply()

  def applyAndCountMatches() = {
    strategies.map(strategy => {
      strategy.apply()
      if (strategy.results.isEmpty) 0 else strategy.results.last._2.matches
    }).sum
  }
}
