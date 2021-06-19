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

package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.field.l4.L4_SlotSpecification
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_UnresolvedAccess

object L4_UnresolvedAccess {
  def apply(name : String) = new L4_UnresolvedAccess(name, None, None, None, None, None)
  def apply(name : String, level : Option[L4_AccessLevelSpecification]) = new L4_UnresolvedAccess(name, level, None, None, None, None)
  def apply(name : String, level : Option[L4_AccessLevelSpecification], slot : Option[L4_SlotSpecification], offset : Option[L4_ConstIndex], dirAccess : Option[L4_ConstIndex], matIndex : Option[L4_MatIndex]) = new L4_UnresolvedAccess(name, level, slot, offset, dirAccess, matIndex)
}

case class L4_UnresolvedAccess(
    var name : String,
    var level : Option[L4_AccessLevelSpecification],
    var slot : Option[L4_SlotSpecification],
    var offset : Option[L4_ConstIndex],
    var dirAccess : Option[L4_ConstIndex],
    var matIndex : Option[L4_MatIndex]
) extends L4_Access with L4_CanBeOffset {

  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '<' << slot.get << '>'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if(matIndex.isDefined) {
      out << matIndex.get
    }
    if (dirAccess.isDefined) out << ':' << dirAccess.get

  }

  override def progress : IR_Expression = ProgressLocation {
    Logger.warn(s"Progressing unresolved access on L4: $name" + (if (level.isDefined) s"@${ level.get }" else ""))

    if (slot.isDefined) Logger.warn("Discarding meaningless slot access on basic or leveled access")
    if (offset.isDefined) Logger.warn("Discarding meaningless offset access on basic or leveled access")
    if (dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on basic or leveled access " + name)
    if (matIndex.isDefined) Logger.warn("Discarding meaningless matrix index access on basic or leveled access")

    if (level.isEmpty)
      IR_StringLiteral(name)
    else
      IR_StringLiteral(name + "_" + level.get.resolveLevel)
  }
}
