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

package exastencils.baseExt.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.field.l3.L3_SlotSpecification
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_UnresolvedAccess

object L3_UnresolvedAccess {
  def apply(name : String) = new L3_UnresolvedAccess(name, None, None, None, None, None)
  def apply(name : String, level : Option[L3_AccessLevelSpecification]) = new L3_UnresolvedAccess(name, level, None, None, None, None)
}

case class L3_UnresolvedAccess(
    var name : String,
    var level : Option[L3_AccessLevelSpecification],
    var slot : Option[L3_SlotSpecification],
    var offset : Option[L3_ConstIndex],
    var dirAccess : Option[L3_ConstIndex],
    var arrayIndex : Option[Int]) extends L3_Access with L3_CanBeOffset {

  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Progressing unresolved access on L3: $name" + (if (level.isDefined) s"@${ level.get }" else ""))

    L4_UnresolvedAccess(
      name,
      L3_ProgressOption(level)(_.progress),
      L3_ProgressOption(slot)(_.progress),
      L3_ProgressOption(offset)(_.progress),
      L3_ProgressOption(dirAccess)(_.progress),
      arrayIndex)
  }
}
