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

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.l3.L3_LevelCollector

/// L3_LevelSpecification

object L3_LevelSpecification {
  def extractLevelList(levels : Option[L3_LevelSpecification], defForNone : List[Int]) : List[Int] = {
    levels match {
      case None                        => defForNone
      case Some(L3_SingleLevel(level)) => List(level)
      case Some(L3_LevelList(lvls))    => lvls.map(_.resolveLevel).toList
      case other                       => Logger.error("Trying to extract level list from unsupported instance " + other)
    }
  }

  // assumes list of all levels as default
  def extractLevelListDefAll(levels : Option[L3_LevelSpecification]) : List[Int] = extractLevelList(levels, (Knowledge.minLevel to Knowledge.maxLevel).toList)

  // assumes empty level list as default
  def extractLevelListDefEmpty(levels : Option[L3_LevelSpecification]) : List[Int] = extractLevelList(levels, List())

  def asSingleLevel(level : Option[L3_LevelSpecification]) : Int = {
    level match {
      case Some(L3_SingleLevel(lvl)) => lvl
      case None                      => Logger.error("Missing level specification")
      case Some(other)               => Logger.error(s"Invalid level specification: $other")
    }
  }
}

trait L3_LevelSpecification extends L3_Node with L3_Progressable with PrettyPrintable {
  override def progress : L4_LevelSpecification
  def resolveLevel : Int
}

/// L3_DeclarationLevelSpecification

// can be used for declarations, e.g., functions
trait L3_DeclarationLevelSpecification extends L3_LevelSpecification {
  override def progress : L4_DeclarationLevelSpecification
}

/// L3_AccessLevelSpecification

// can be used for accesses, e.g., in fields or function calls
trait L3_AccessLevelSpecification extends L3_LevelSpecification {
  override def progress : L4_AccessLevelSpecification
}

/// L3_ResolveLevelSpecifications

object L3_ResolveLevelSpecifications extends DefaultStrategy("Resolve level specifications") {
  // resolve level identifiers "coarsest", "finest"
  this += new Transformation("Resolve first batch of level aliases", {
    case L3_CoarsestLevel => L3_SingleLevel(Knowledge.minLevel)
    case L3_FinestLevel   => L3_SingleLevel(Knowledge.maxLevel)
    case L3_AllLevels     => L3_LevelRange(L3_SingleLevel(Knowledge.minLevel), L3_SingleLevel(Knowledge.maxLevel))
  })

  // resolve relative level identifiers
  this += new Transformation("Resolve relative level specifications", {
    case L3_RelativeLevel(L3_SingleLevel(level), "+", offset) => L3_SingleLevel(level + offset)
    case L3_RelativeLevel(L3_SingleLevel(level), "-", offset) => L3_SingleLevel(level - offset)
    case level : L3_RelativeLevel                             => Logger.error("Unsupported variant of L3_RelativeLevel found: " + level)
  })

  // convert level ranges to level lists
  this += new Transformation("Resolve level ranges", {
    case L3_LevelRange(L3_SingleLevel(begin), L3_SingleLevel(end)) => L3_LevelList((begin to end).map(L3_SingleLevel).toList)
    case levels : L3_LevelRange                                    => Logger.error("Unsupported variant of L3_LevelRange found: " + levels)
  })

  // flatten level lists and incorporate negated level lists
  this += new Transformation("Process level lists", {
    case levels : L3_LevelList =>
      // resolve lists of lists
      levels.flatten()

      // resolve negations/ level exclusions
      // TODO: will this work if elements of x occurs multiple times?
      levels.levels.foreach {
        case elem @ L3_NegatedLevelList(L3_LevelList(x)) =>
          levels.levels --= x
          levels.levels.remove(elem)
        case _                                           =>
      }

      levels
  })
}

/// L3_ResolveRelativeLevels

object L3_ResolveRelativeLevels extends DefaultStrategy("Resolve relative level specifications") {
  val collector = new L3_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def getLevel() : Int = {
    if (collector.inLevelScope)
      collector.getCurrentLevel
    else
      Logger.error("Trying to access current outside of a valid level scope")
  }

  // resolve level identifiers "coarsest", "finest"
  this += new Transformation("Resolve relative level aliases", {
    case L3_CurrentLevel => L3_SingleLevel(getLevel())
    case L3_CoarserLevel => L3_SingleLevel(getLevel() - 1)
    case L3_FinerLevel   => L3_SingleLevel(getLevel() + 1)
  })
}
