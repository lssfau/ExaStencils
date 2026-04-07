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

package exastencils.util.ir

import scala.collection.mutable.Stack

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger.Logger

/// IR_LevelCollector

class IR_LevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  def enterLevel(level : Int) = levelStack.push(level)
  def leaveLevel(level : Int) = levelStack.pop()

  override def enter(node : Node) : Unit = {
    node match {
      case fct : IR_LeveledFunctionLike          => enterLevel(fct.level)
      case loop : IR_LoopOverPoints              => enterLevel(loop.field.level)
      case loop : IR_LoopOverPointsInOneFragment => enterLevel(loop.field.level)

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case fct : IR_LeveledFunctionLike          => leaveLevel(fct.level)
      case loop : IR_LoopOverPoints              => leaveLevel(loop.field.level)
      case loop : IR_LoopOverPointsInOneFragment => leaveLevel(loop.field.level)

      case _ =>
    }
  }

  override def reset() : Unit = {
    levelStack.clear
  }

  def inLevelScope : Boolean = levelStack.nonEmpty

  def getCurrentLevel : Int = {
    if (levelStack.isEmpty) {
      Logger.dbg("Trying to access level outside of a valid scope")
      -1
    } else {
      levelStack.head
    }
  }
}
