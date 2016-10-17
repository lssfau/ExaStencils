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
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "all"
}

/// L4_LevelRange

case class L4_LevelRange(var begin : L4_LevelSpecification, var end : L4_LevelSpecification) extends L4_LevelGroup {
  def prettyprint(out : PpStream) = out << '(' << begin << " to " << end << ')'
}

/// L4_LevelList

object L4_LevelList {
  def apply() = new L4_LevelList(HashSet())
  def apply(level : L4_LevelSpecification) = new L4_LevelList(HashSet(level))
  def apply(levels : List[L4_LevelSpecification]) = new L4_LevelList(levels.to[HashSet])
}

case class L4_LevelList(var levels : HashSet[L4_LevelSpecification]) extends L4_LevelGroup {
  override def prettyprint(out : PpStream) = out << '(' <<< (levels, ", ") << ')'

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

case class L4_NegatedLevelList(var levels : L4_LevelList) extends L4_LevelGroup {
  def prettyprint(out : PpStream) = out << "not" << levels
}
