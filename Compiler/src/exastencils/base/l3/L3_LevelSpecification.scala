package exastencils.base.l3

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L3_LevelSpecification

object L3_LevelSpecification {
  def extractLevelList(levels : Option[L3_LevelSpecification], defForNone : List[Int]) : List[Int] = {
    levels match {
      case None                        => defForNone
      case Some(L3_SingleLevel(level)) => List(level)
      case Some(L3_LevelList(levels))  => levels.map(_.asInstanceOf[L3_SingleLevel].level).toList
    }
  }

  // assumes list of all levels as default
  def extractLevelListDefAll(levels : Option[L3_LevelSpecification]) : List[Int] = extractLevelList(levels, (Knowledge.minLevel to Knowledge.maxLevel).toList)

  // assumes empty level list as default
  def extractLevelListDefEmpty(levels : Option[L3_LevelSpecification]) : List[Int] = extractLevelList(levels, List())
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
