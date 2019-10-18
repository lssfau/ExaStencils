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

/// L4_SingleLevel

case class L4_SingleLevel(var level : Int) extends L4_DeclarationLevelSpecification with L4_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << level
  override def toString = level.toString
  override def resolveLevel : Int = level
}

/// L4_RelativeLevel

// FIXME: op -> BinOp
case class L4_RelativeLevel(var base : L4_LevelSpecification, var op : String, var offset : Int) extends L4_DeclarationLevelSpecification with L4_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << '(' << base << ' ' << op << ' ' << offset << ')'
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
}
