package exastencils.base.l1

import scala.collection.mutable.HashSet

import exastencils.base.l2._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_LevelGroup

trait L1_LevelGroup extends L1_DeclarationLevelSpecification {
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
}

/// L1_AllLevels

case object L1_AllLevels extends L1_LevelGroup {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "all"
  override def progress = L2_AllLevels
}

/// L1_LevelRange

case class L1_LevelRange(var begin : L1_LevelSpecification, var end : L1_LevelSpecification) extends L1_LevelGroup {
  def prettyprint(out : PpStream) = out << '(' << begin << " to " << end << ')'
  override def progress = L2_LevelRange(begin.progress, end.progress)
}

/// L1_LevelList

object L1_LevelList {
  def apply() = new L1_LevelList(HashSet())
  def apply(level : L1_DeclarationLevelSpecification) = new L1_LevelList(HashSet(level))
  def apply(levels : List[L1_DeclarationLevelSpecification]) = new L1_LevelList(levels.to[HashSet])
}

case class L1_LevelList(var levels : HashSet[L1_DeclarationLevelSpecification]) extends L1_LevelGroup {
  override def prettyprint(out : PpStream) = {
    val (first, second) = levels.partition(!_.isInstanceOf[L1_NegatedLevelList])
    out << "(" <<< (first, ", ")
    if (second.size > 1) Logger.error("More than one negation per level list is not supported")
    if (second.nonEmpty) out << " " << second.head
    out << ")"
  }

  override def progress = L2_LevelList(levels.map(_.progress))

  def flatten() : Unit = {
    levels.foreach {
      case elem @ L1_LevelList(x) =>
        levels.++=(x)
        levels.remove(elem)
      case _                      =>
    }
  }

  def contains(level : Int) = levels.exists({ case L1_SingleLevel(`level`) => true; case _ => false })
}

/// L1_NegatedLevelList

object L1_NegatedLevelList {
  def apply() = new L1_NegatedLevelList(L1_LevelList())
  def apply(level : L1_DeclarationLevelSpecification) = new L1_NegatedLevelList(L1_LevelList(level))
  def apply(levels : List[L1_DeclarationLevelSpecification]) = new L1_NegatedLevelList(L1_LevelList(levels))
}

case class L1_NegatedLevelList(var levels : L1_LevelList) extends L1_LevelGroup {
  def prettyprint(out : PpStream) = out << "but " << levels
  override def progress = L2_NegatedLevelList(levels.progress)
}
