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

package exastencils.util.l2

import scala.collection.mutable.Stack

import exastencils.base.l2._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.knowledge.l2._
import exastencils.logger.Logger

/// L2_LevelCollector

class L2_LevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  def enterLevel(level : Option[L2_LevelSpecification]) = {
    level match {
      case Some(L2_SingleLevel(lvl)) => levelStack.push(lvl)
      case _                         =>
    }
  }
  def leaveLevel(level : Option[L2_LevelSpecification]) = {
    level match {
      case Some(L2_SingleLevel(_)) => levelStack.pop()
      case _                       =>
    }
  }

  override def enter(node : Node) : Unit = {
    node match {
      case decl : L2_LeveledKnowledgeDecl => enterLevel(decl.levels)
      case _                              =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case decl : L2_LeveledKnowledgeDecl => leaveLevel(decl.levels)
      case _                              =>
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
