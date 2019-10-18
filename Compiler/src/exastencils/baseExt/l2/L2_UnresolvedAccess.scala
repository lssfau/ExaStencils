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

package exastencils.baseExt.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.field.l2.L2_SlotSpecification
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_UnresolvedAccess

object L2_UnresolvedAccess {
  def apply(name : String) = new L2_UnresolvedAccess(name, None, None, None, None, None)
  def apply(name : String, level : Option[L2_AccessLevelSpecification]) = new L2_UnresolvedAccess(name, level, None, None, None, None)
}

case class L2_UnresolvedAccess(
    var name : String,
    var level : Option[L2_AccessLevelSpecification],
    var slot : Option[L2_SlotSpecification],
    var offset : Option[L2_ConstIndex],
    var dirAccess : Option[L2_ConstIndex],
    var arrayIndex : Option[Int]) extends L2_Access with L2_CanBeOffset {

  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Progressing unresolved access on L2: $name" + (if (level.isDefined) s"@${ level.get }" else ""))

    L3_UnresolvedAccess(
      name,
      L2_ProgressOption(level)(_.progress),
      L2_ProgressOption(slot)(_.progress),
      L2_ProgressOption(offset)(_.progress),
      L2_ProgressOption(dirAccess)(_.progress),
      arrayIndex)
  }
}
