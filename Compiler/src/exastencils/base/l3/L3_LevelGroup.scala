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

package exastencils.base.l3

import scala.collection.mutable.HashSet

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_LevelGroup

trait L3_LevelGroup extends L3_DeclarationLevelSpecification {
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
}

/// L3_AllLevels

case object L3_AllLevels extends L3_LevelGroup {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "all"
  override def progress = ProgressLocation(L4_AllLevels)
}

/// L3_LevelRange

case class L3_LevelRange(var begin : L3_LevelSpecification, var end : L3_LevelSpecification) extends L3_LevelGroup {
  def prettyprint(out : PpStream) = out << '(' << begin << " to " << end << ')'
  override def progress = ProgressLocation(L4_LevelRange(begin.progress, end.progress))
}

/// L3_LevelList

object L3_LevelList {
  def apply() = new L3_LevelList(HashSet())
  def apply(level : L3_DeclarationLevelSpecification) = new L3_LevelList(HashSet(level))
  def apply(levels : List[L3_DeclarationLevelSpecification]) = new L3_LevelList(levels.to[HashSet])
}

case class L3_LevelList(var levels : HashSet[L3_DeclarationLevelSpecification]) extends L3_LevelGroup {
  override def prettyprint(out : PpStream) = {
    val (first, second) = levels.partition(!_.isInstanceOf[L3_NegatedLevelList])
    out << "(" <<< (first, ", ")
    if (second.size > 1) Logger.error("More than one negation per level list is not supported")
    if (second.nonEmpty) out << " " << second.head
    out << ")"
  }

  override def progress = ProgressLocation(L4_LevelList(levels.map(_.progress)))

  def flatten() : Unit = {
    levels.foreach {
      case elem @ L3_LevelList(x) =>
        levels.++=(x)
        levels.remove(elem)
      case _                      =>
    }
  }

  def contains(level : Int) = levels.exists({ case L3_SingleLevel(`level`) => true; case _ => false })
}

/// L3_NegatedLevelList

object L3_NegatedLevelList {
  def apply() = new L3_NegatedLevelList(L3_LevelList())
  def apply(level : L3_DeclarationLevelSpecification) = new L3_NegatedLevelList(L3_LevelList(level))
  def apply(levels : List[L3_DeclarationLevelSpecification]) = new L3_NegatedLevelList(L3_LevelList(levels))
}

case class L3_NegatedLevelList(var levels : L3_LevelList) extends L3_LevelGroup {
  def prettyprint(out : PpStream) = out << "but " << levels
  override def progress = ProgressLocation(L4_NegatedLevelList(levels.progress))
}
