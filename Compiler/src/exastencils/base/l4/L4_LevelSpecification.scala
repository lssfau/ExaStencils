package exastencils.base.l4

import exastencils.config.Knowledge
import exastencils.core.collectors.L4LevelCollector
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_LevelSpecification

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

/// L4_ResolveCurrentLevels

object L4_ResolveCurrentLevels extends DefaultStrategy("Resolve current level references") {
  var levelCollector = new L4LevelCollector
  this.register(levelCollector)

  // resolve level specifications
  this += new Transformation("Resolve relative level specifications", {
    case L4_CurrentLevel => L4_SingleLevel(levelCollector.getCurrentLevel)
    case L4_CoarserLevel => L4_SingleLevel(levelCollector.getCurrentLevel - 1)
    case L4_FinerLevel   => L4_SingleLevel(levelCollector.getCurrentLevel + 1)
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
