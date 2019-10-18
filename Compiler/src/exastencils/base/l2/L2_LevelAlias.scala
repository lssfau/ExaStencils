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

package exastencils.base.l2

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_LevelAlias

trait L2_LevelAlias extends L2_DeclarationLevelSpecification with L2_AccessLevelSpecification {
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
  override def progress : L3_DeclarationLevelSpecification with L3_AccessLevelSpecification
}

/// L2_CurrentLevel

case object L2_CurrentLevel extends L2_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "current"
  override def progress = ProgressLocation(L3_CurrentLevel)
}

/// L2_CoarserLevel

case object L2_CoarserLevel extends L2_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "coarser"
  override def progress = ProgressLocation(L3_CoarserLevel)
}

/// L2_FinerLevel

case object L2_FinerLevel extends L2_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "finer"
  override def progress = ProgressLocation(L3_FinerLevel)
}

/// L2_CoarsestLevel

case object L2_CoarsestLevel extends L2_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "coarsest"
  override def progress = ProgressLocation(L3_CoarsestLevel)
}

/// L2_FinestLevel

case object L2_FinestLevel extends L2_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "finest"
  override def progress = ProgressLocation(L3_FinestLevel)
}
