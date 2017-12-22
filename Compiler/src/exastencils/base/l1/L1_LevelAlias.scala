package exastencils.base.l1

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_LevelAlias

trait L1_LevelAlias extends L1_DeclarationLevelSpecification with L1_AccessLevelSpecification {
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
  override def progress : L2_DeclarationLevelSpecification with L2_AccessLevelSpecification
}

/// L1_CurrentLevel

case object L1_CurrentLevel extends L1_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "current"
  override def progress = ProgressLocation(L2_CurrentLevel)
}

/// L1_CoarserLevel

case object L1_CoarserLevel extends L1_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "coarser"
  override def progress = ProgressLocation(L2_CoarserLevel)
}

/// L1_FinerLevel

case object L1_FinerLevel extends L1_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "finer"
  override def progress = ProgressLocation(L2_FinerLevel)
}

/// L1_CoarsestLevel

case object L1_CoarsestLevel extends L1_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "coarsest"
  override def progress = ProgressLocation(L2_CoarsestLevel)
}

/// L1_FinestLevel

case object L1_FinestLevel extends L1_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "finest"
  override def progress = ProgressLocation(L2_FinestLevel)
}
