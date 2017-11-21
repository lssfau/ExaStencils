package exastencils.base.l2

import scala.collection.mutable.HashSet

import exastencils.base.l3._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_LevelGroup

trait L2_LevelGroup extends L2_DeclarationLevelSpecification {
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
}

/// L2_AllLevels

case object L2_AllLevels extends L2_LevelGroup {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "all"
  override def progress = L3_AllLevels
}

/// L2_LevelRange

case class L2_LevelRange(var begin : L2_LevelSpecification, var end : L2_LevelSpecification) extends L2_LevelGroup {
  def prettyprint(out : PpStream) = out << '(' << begin << " to " << end << ')'
  override def progress = L3_LevelRange(begin.progress, end.progress)
}

/// L2_LevelList

object L2_LevelList {
  def apply() = new L2_LevelList(HashSet())
  def apply(level : L2_DeclarationLevelSpecification) = new L2_LevelList(HashSet(level))
  def apply(levels : List[L2_DeclarationLevelSpecification]) = new L2_LevelList(levels.to[HashSet])
}

case class L2_LevelList(var levels : HashSet[L2_DeclarationLevelSpecification]) extends L2_LevelGroup {
  override def prettyprint(out : PpStream) = {
    val (first, second) = levels.partition(!_.isInstanceOf[L2_NegatedLevelList])
    out << "(" <<< (first, ", ")
    if (second.size > 1) Logger.error("More than one negation per level list is not supported")
    if (second.nonEmpty) out << " " << second.head
    out << ")"
  }

  override def progress = L3_LevelList(levels.map(_.progress))

  def flatten() : Unit = {
    levels.foreach {
      case elem @ L2_LevelList(x) =>
        levels.++=(x)
        levels.remove(elem)
      case _                      =>
    }
  }

  def contains(level : Int) = levels.exists({ case L2_SingleLevel(`level`) => true; case _ => false })
}

/// L2_NegatedLevelList

object L2_NegatedLevelList {
  def apply() = new L2_NegatedLevelList(L2_LevelList())
  def apply(level : L2_DeclarationLevelSpecification) = new L2_NegatedLevelList(L2_LevelList(level))
  def apply(levels : List[L2_DeclarationLevelSpecification]) = new L2_NegatedLevelList(L2_LevelList(levels))
}

case class L2_NegatedLevelList(var levels : L2_LevelList) extends L2_LevelGroup {
  def prettyprint(out : PpStream) = out << "but " << levels
  override def progress = L3_NegatedLevelList(levels.progress)
}
