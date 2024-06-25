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

package exastencils.timing.ir

import scala.collection.mutable.HashMap

import exastencils.datastructures._

/// IR_CollectTimers

object IR_CollectTimers extends DefaultStrategy("Collect all timers used") {
  var timers : HashMap[String, IR_TimingIV] = HashMap()

  override def apply(node : Option[Node] = None) = {
    timers.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    timers.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collect", {
    case timer : IR_TimingIV           =>
      timers += (timer.resolveName -> timer)
      timer
    /*case timer : IR_PlainTimingIV           =>
      timers += (timer.resolveName -> timer)
      timer
    case leveled_timer : IR_LeveledTimingIV =>
      timers += (leveled_timer.resolveName() -> leveled_timer)
      leveled_timer*/
  })
}
