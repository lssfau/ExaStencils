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

package exastencils.base.l4

import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_LevelAlias

trait L4_LevelAlias extends L4_DeclarationLevelSpecification with L4_AccessLevelSpecification {
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
}

/// L4_CurrentLevel

case object L4_CurrentLevel extends L4_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "current"
}

/// L4_CoarserLevel

case object L4_CoarserLevel extends L4_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "coarser"
}

/// L4_FinerLevel

case object L4_FinerLevel extends L4_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "finer"
}

/// L4_CoarsestLevel

case object L4_CoarsestLevel extends L4_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "coarsest"
}

/// L4_FinestLevel

case object L4_FinestLevel extends L4_LevelAlias {
  exastencils.core.Duplicate.registerConstant(this)
  def prettyprint(out : PpStream) = out << "finest"
}
