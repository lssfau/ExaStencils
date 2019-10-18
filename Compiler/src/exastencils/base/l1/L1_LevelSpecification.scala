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

package exastencils.base.l1

import exastencils.base.l2._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.l1.L1_LevelCollector

/// L1_LevelSpecification

object L1_LevelSpecification {
  def extractLevelList(levels : Option[L1_LevelSpecification], defForNone : List[Int]) : List[Int] = {
    levels match {
      case None                        => defForNone
      case Some(L1_SingleLevel(level)) => List(level)
      case Some(L1_LevelList(lvls))    => lvls.map(_.resolveLevel).toList
      case other                       => Logger.error("Trying to extract level list from unsupported instance " + other)
    }
  }

  // assumes list of all levels as default
  def extractLevelListDefAll(levels : Option[L1_LevelSpecification]) : List[Int] = extractLevelList(levels, (Knowledge.minLevel to Knowledge.maxLevel).toList)

  // assumes empty level list as default
  def extractLevelListDefEmpty(levels : Option[L1_LevelSpecification]) : List[Int] = extractLevelList(levels, List())

  def asSingleLevel(level : Option[L1_LevelSpecification]) : Int = {
    level match {
      case Some(L1_SingleLevel(lvl)) => lvl
      case None                      => Logger.error("Missing level specification")
      case Some(other)               => Logger.error(s"Invalid level specification: $other")
    }
  }
}

trait L1_LevelSpecification extends L1_Node with L1_Progressable with PrettyPrintable {
  override def progress : L2_LevelSpecification
  def resolveLevel : Int
}

/// L1_DeclarationLevelSpecification

// can be used for declarations, e.g., functions
trait L1_DeclarationLevelSpecification extends L1_LevelSpecification {
  override def progress : L2_DeclarationLevelSpecification
}

/// L1_AccessLevelSpecification

// can be used for accesses, e.g., in fields or function calls
trait L1_AccessLevelSpecification extends L1_LevelSpecification {
  override def progress : L2_AccessLevelSpecification
}

/// L1_ResolveLevelSpecifications

object L1_ResolveLevelSpecifications extends DefaultStrategy("Resolve level specifications") {
  // resolve level identifiers "coarsest", "finest"
  this += new Transformation("Resolve first batch of level aliases", {
    case L1_CoarsestLevel => L1_SingleLevel(Knowledge.minLevel)
    case L1_FinestLevel   => L1_SingleLevel(Knowledge.maxLevel)
    case L1_AllLevels     => L1_LevelRange(L1_SingleLevel(Knowledge.minLevel), L1_SingleLevel(Knowledge.maxLevel))
  })

  // resolve relative level identifiers
  this += new Transformation("Resolve relative level specifications", {
    case L1_RelativeLevel(L1_SingleLevel(level), "+", offset) => L1_SingleLevel(level + offset)
    case L1_RelativeLevel(L1_SingleLevel(level), "-", offset) => L1_SingleLevel(level - offset)
    case level : L1_RelativeLevel                             => Logger.error("Unsupported variant of L1_RelativeLevel found: " + level)
  })

  // convert level ranges to level lists
  this += new Transformation("Resolve level ranges", {
    case L1_LevelRange(L1_SingleLevel(begin), L1_SingleLevel(end)) => L1_LevelList((begin to end).map(L1_SingleLevel).toList)
    case levels : L1_LevelRange                                    => Logger.error("Unsupported variant of L1_LevelRange found: " + levels)
  })

  // flatten level lists and incorporate negated level lists
  this += new Transformation("Process level lists", {
    case levels : L1_LevelList =>
      // resolve lists of lists
      levels.flatten()

      // resolve negations/ level exclusions
      // TODO: will this work if elements of x occurs multiple times?
      levels.levels.foreach {
        case elem @ L1_NegatedLevelList(L1_LevelList(x)) =>
          levels.levels --= x
          levels.levels.remove(elem)
        case _                                           =>
      }

      levels
  })
}

/// L1_ResolveRelativeLevels

object L1_ResolveRelativeLevels extends DefaultStrategy("Resolve relative level specifications") {
  val collector = new L1_LevelCollector
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
    case L1_CurrentLevel => L1_SingleLevel(getLevel())
    case L1_CoarserLevel => L1_SingleLevel(getLevel() - 1)
    case L1_FinerLevel   => L1_SingleLevel(getLevel() + 1)
  })
}
