package exastencils.base.l2

import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L2_LevelSpecification

object L2_LevelSpecification {
  def extractLevelList(levels : Option[L2_LevelSpecification], defForNone : List[Int]) : List[Int] = {
    levels match {
      case None                        => defForNone
      case Some(L2_SingleLevel(level)) => List(level)
      case Some(L2_LevelList(levels))  => levels.map(_.asInstanceOf[L2_SingleLevel].level).toList
    }
  }

  // assumes list of all levels as default
  def extractLevelListDefAll(levels : Option[L2_LevelSpecification]) : List[Int] = extractLevelList(levels, (Knowledge.minLevel to Knowledge.maxLevel).toList)

  // assumes empty level list as default
  def extractLevelListDefEmpty(levels : Option[L2_LevelSpecification]) : List[Int] = extractLevelList(levels, List())
}

trait L2_LevelSpecification extends L2_Node with L2_Progressable with PrettyPrintable {
  override def progress : L3_LevelSpecification
  def resolveLevel : Int
}

/// L2_DeclarationLevelSpecification

// can be used for declarations, e.g., functions
trait L2_DeclarationLevelSpecification extends L2_LevelSpecification {
  override def progress : L3_DeclarationLevelSpecification
}

/// L2_AccessLevelSpecification

// can be used for accesses, e.g., in fields or function calls
trait L2_AccessLevelSpecification extends L2_LevelSpecification {
  override def progress : L3_AccessLevelSpecification
}

/// L2_ResolveLevelSpecifications

object L2_ResolveLevelSpecifications extends DefaultStrategy("Resolve level specifications") {
  // resolve level identifiers "coarsest", "finest"
  this += new Transformation("Resolve first batch of level aliases", {
    case L2_CoarsestLevel => L2_SingleLevel(Knowledge.minLevel)
    case L2_FinestLevel   => L2_SingleLevel(Knowledge.maxLevel)
    case L2_AllLevels     => L2_LevelRange(L2_SingleLevel(Knowledge.minLevel), L2_SingleLevel(Knowledge.maxLevel))
  })

  // resolve relative level identifiers
  this += new Transformation("Resolve relative level specifications", {
    case L2_RelativeLevel(L2_SingleLevel(level), "+", offset) => L2_SingleLevel(level + offset)
    case L2_RelativeLevel(L2_SingleLevel(level), "-", offset) => L2_SingleLevel(level - offset)
    case level : L2_RelativeLevel                             => Logger.error("Unsupported variant of L2_RelativeLevel found: " + level)
  })

  // convert level ranges to level lists
  this += new Transformation("Resolve level ranges", {
    case L2_LevelRange(L2_SingleLevel(begin), L2_SingleLevel(end)) => L2_LevelList((begin to end).map(L2_SingleLevel).toList)
    case levels : L2_LevelRange                                    => Logger.error("Unsupported variant of L2_LevelRange found: " + levels)
  })

  // flatten level lists and incorporate negated level lists
  this += new Transformation("Process level lists", {
    case levels : L2_LevelList =>
      // resolve lists of lists
      levels.flatten()

      // resolve negations/ level exclusions
      // TODO: will this work if elements of x occurs multiple times?
      levels.levels.foreach {
        case elem @ L2_NegatedLevelList(L2_LevelList(x)) =>
          levels.levels --= x
          levels.levels.remove(elem)
        case _                                           =>
      }

      levels
  })
}