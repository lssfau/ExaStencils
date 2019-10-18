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

/// L3_SingleLevel

case class L3_SingleLevel(var level : Int) extends L3_DeclarationLevelSpecification with L3_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << level
  override def toString = level.toString
  override def resolveLevel : Int = level
  override def progress = ProgressLocation(L4_SingleLevel(level))
}

/// L3_RelativeLevel

// FIXME: op -> BinOp
case class L3_RelativeLevel(var base : L3_LevelSpecification, var op : String, var offset : Int) extends L3_DeclarationLevelSpecification with L3_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << '(' << base << ' ' << op << ' ' << offset << ')'
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
  override def progress = ProgressLocation(L4_RelativeLevel(base.progress, op, offset))
}
