package exastencils.base.l3

import exastencils.base.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_LevelAlias

trait L3_LevelAlias extends L3_DeclarationLevelSpecification with L3_AccessLevelSpecification {
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
  override def progress : L4_DeclarationLevelSpecification with L4_AccessLevelSpecification
}

/// L3_CurrentLevel

case object L3_CurrentLevel extends L3_LevelAlias {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "current"
  override def progress = L4_CurrentLevel
}

/// L3_CoarserLevel

case object L3_CoarserLevel extends L3_LevelAlias {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "coarser"
  override def progress = L4_CoarserLevel
}

/// L3_FinerLevel

case object L3_FinerLevel extends L3_LevelAlias {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "finer"
  override def progress = L4_FinerLevel
}

/// L3_CoarsestLevel

case object L3_CoarsestLevel extends L3_LevelAlias {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "coarsest"
  override def progress = L4_CoarsestLevel
}

/// L3_FinestLevel

case object L3_FinestLevel extends L3_LevelAlias {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "finest"
  override def progress = L4_FinestLevel
}
