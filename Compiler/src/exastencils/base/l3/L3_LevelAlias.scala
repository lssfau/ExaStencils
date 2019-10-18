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

package exastencils.base.l3

import exastencils.base.ProgressLocation
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
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "current"
  override def progress = ProgressLocation(L4_CurrentLevel)
}

/// L3_CoarserLevel

case object L3_CoarserLevel extends L3_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "coarser"
  override def progress = ProgressLocation(L4_CoarserLevel)
}

/// L3_FinerLevel

case object L3_FinerLevel extends L3_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "finer"
  override def progress = ProgressLocation(L4_FinerLevel)
}

/// L3_CoarsestLevel

case object L3_CoarsestLevel extends L3_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "coarsest"
  override def progress = ProgressLocation(L4_CoarsestLevel)
}

/// L3_FinestLevel

case object L3_FinestLevel extends L3_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "finest"
  override def progress = ProgressLocation(L4_FinestLevel)
}
