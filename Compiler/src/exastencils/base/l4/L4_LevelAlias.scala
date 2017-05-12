package exastencils.base.l4

import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_LevelAlias

trait L4_LevelAlias extends L4_DeclarationLevelSpecification with L4_AccessLevelSpecification {
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
}

/// L4_CurrentLevel

case object L4_CurrentLevel extends L4_LevelAlias {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "current"
}

/// L4_CoarserLevel

case object L4_CoarserLevel extends L4_LevelAlias {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "coarser"
}

/// L4_FinerLevel

case object L4_FinerLevel extends L4_LevelAlias {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "finer"
}

/// L4_CoarsestLevel

case object L4_CoarsestLevel extends L4_LevelAlias {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "coarsest"
}

/// L4_FinestLevel

case object L4_FinestLevel extends L4_LevelAlias {
  exastencils.core.Duplicate.registerImmutable(this.getClass)
  def prettyprint(out : PpStream) = out << "finest"
}
