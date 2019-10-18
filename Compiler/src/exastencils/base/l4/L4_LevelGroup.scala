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

package exastencils.base.l4

import scala.collection.mutable.HashSet

import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_LevelGroup

trait L4_LevelGroup extends L4_DeclarationLevelSpecification {
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
}

/// L4_AllLevels

case object L4_AllLevels extends L4_LevelGroup {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "all"
}

/// L4_LevelRange

case class L4_LevelRange(var begin : L4_LevelSpecification, var end : L4_LevelSpecification) extends L4_LevelGroup {
  def prettyprint(out : PpStream) = out << '(' << begin << " to " << end << ')'
}

/// L4_LevelList

object L4_LevelList {
  def apply() = new L4_LevelList(HashSet())
  def apply(level : L4_DeclarationLevelSpecification) = new L4_LevelList(HashSet(level))
  def apply(levels : List[L4_DeclarationLevelSpecification]) = new L4_LevelList(levels.to[HashSet])
}

case class L4_LevelList(var levels : HashSet[L4_DeclarationLevelSpecification]) extends L4_LevelGroup {
  override def prettyprint(out : PpStream) = {
    val (first, second) = levels.partition(!_.isInstanceOf[L4_NegatedLevelList])
    out << "(" <<< (first, ", ")
    if (second.size > 1) Logger.error("More than one negation per level list is not supported")
    if (second.nonEmpty) out << " " << second.head
    out << ")"
  }

  def flatten() : Unit = {
    levels.foreach {
      case elem @ L4_LevelList(x) =>
        levels.++=(x)
        levels.remove(elem)
      case _                      =>
    }
  }

  def contains(level : Int) = levels.exists({ case L4_SingleLevel(`level`) => true; case _ => false })
}

/// L4_NegatedLevelList

object L4_NegatedLevelList {
  def apply() = new L4_NegatedLevelList(L4_LevelList())
  def apply(level : L4_DeclarationLevelSpecification) = new L4_NegatedLevelList(L4_LevelList(level))
  def apply(levels : List[L4_DeclarationLevelSpecification]) = new L4_NegatedLevelList(L4_LevelList(levels))
}

case class L4_NegatedLevelList(var levels : L4_LevelList) extends L4_LevelGroup {
  def prettyprint(out : PpStream) = out << "but " << levels
}
