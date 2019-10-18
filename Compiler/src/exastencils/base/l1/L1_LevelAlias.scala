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
