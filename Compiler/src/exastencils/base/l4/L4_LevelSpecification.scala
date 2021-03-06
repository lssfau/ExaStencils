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

import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.l4.L4_LevelCollector

/// L4_LevelSpecification

object L4_LevelSpecification {
  def extractLevelList(levels : Option[L4_LevelSpecification], defForNone : List[Int]) : List[Int] = {
    levels match {
      case None                        => defForNone
      case Some(L4_SingleLevel(level)) => List(level)
      case Some(L4_LevelList(lvls))    => lvls.map(_.resolveLevel).toList
      case other                       => Logger.error("Trying to extract level list from unsupported instance " + other)
    }
  }

  // assumes list of all levels as default
  def extractLevelListDefAll(levels : Option[L4_LevelSpecification]) : List[Int] = extractLevelList(levels, (Knowledge.minLevel to Knowledge.maxLevel).toList)

  // assumes empty level list as default
  def extractLevelListDefEmpty(levels : Option[L4_LevelSpecification]) : List[Int] = extractLevelList(levels, List())

  def asSingleLevel(level : Option[L4_LevelSpecification]) : Int = {
    level match {
      case Some(L4_SingleLevel(lvl)) => lvl
      case None                      => Logger.error("Missing level specification")
      case Some(other)               => Logger.error(s"Invalid level specification: $other")
    }
  }
}

trait L4_LevelSpecification extends L4_Node with PrettyPrintable {
  def resolveLevel : Int
}

/// L4_DeclarationLevelSpecification

// can be used for declarations, e.g., functions
trait L4_DeclarationLevelSpecification extends L4_LevelSpecification

/// L4_AccessLevelSpecification

// can be used for accesses, e.g., in fields or function calls
trait L4_AccessLevelSpecification extends L4_LevelSpecification

/// L4_ResolveLevelSpecifications

object L4_ResolveLevelSpecifications extends DefaultStrategy("Resolve level specifications") {
  // resolve level identifiers "coarsest", "finest"
  this += new Transformation("Resolve first batch of level aliases", {
    case L4_CoarsestLevel => L4_SingleLevel(Knowledge.minLevel)
    case L4_FinestLevel   => L4_SingleLevel(Knowledge.maxLevel)
    case L4_AllLevels     => L4_LevelRange(L4_SingleLevel(Knowledge.minLevel), L4_SingleLevel(Knowledge.maxLevel))
  })

  // resolve relative level identifiers
  this += new Transformation("Resolve relative level specifications", {
    case L4_RelativeLevel(L4_SingleLevel(level), "+", offset) => L4_SingleLevel(level + offset)
    case L4_RelativeLevel(L4_SingleLevel(level), "-", offset) => L4_SingleLevel(level - offset)
    case level : L4_RelativeLevel                             => Logger.error("Unsupported variant of L4_RelativeLevel found: " + level)
  })

  // convert level ranges to level lists
  this += new Transformation("Resolve level ranges", {
    case L4_LevelRange(L4_SingleLevel(begin), L4_SingleLevel(end)) => L4_LevelList((begin to end).map(L4_SingleLevel).toList)
    case levels : L4_LevelRange                                    => Logger.error("Unsupported variant of L4_LevelRange found: " + levels)
  })

  // flatten level lists and incorporate negated level lists
  this += new Transformation("Process level lists", {
    case levels : L4_LevelList =>
      // resolve lists of lists
      levels.flatten()

      // resolve negations/ level exclusions
      // TODO: will this work if elements of x occurs multiple times?
      levels.levels.foreach {
        case elem @ L4_NegatedLevelList(L4_LevelList(x)) =>
          levels.levels --= x
          levels.levels.remove(elem)
        case _                                           =>
      }

      levels
  })
}

/// L4_ResolveRelativeLevels

object L4_ResolveRelativeLevels extends DefaultStrategy("Resolve relative level specifications") {
  val collector = new L4_LevelCollector
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
    case L4_CurrentLevel => L4_SingleLevel(getLevel())
    case L4_CoarserLevel => L4_SingleLevel(getLevel() - 1)
    case L4_FinerLevel   => L4_SingleLevel(getLevel() + 1)
  })
}

/// L4_ReplaceExplicitLevelsWithCurrent

object L4_ReplaceExplicitLevelsWithCurrent extends QuietDefaultStrategy("Replace explicit levels with CurrentLevel, CoarserLevel and FinerLevel") {
  var curLevel : Int = 0

  this += new Transformation("Replace", {
    case L4_SingleLevel(level) if level == curLevel     => L4_CurrentLevel
    case L4_SingleLevel(level) if level == curLevel - 1 => L4_CoarserLevel
    case L4_SingleLevel(level) if level == curLevel + 1 => L4_FinerLevel
  })
}
