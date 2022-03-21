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

package exastencils.field.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.field.l3._
import exastencils.knowledge.l2._
import exastencils.prettyprinting.PpStream

/// L2_FieldAccess

object L2_FieldAccess {
  def apply(access : L2_FutureFieldAccess) =
    new L2_FieldAccess(L2_FieldCollection.getByIdentifier(access.name, access.level).get, access.offset, access.frozen)
}

case class L2_FieldAccess(
    var target : L2_Field,
    var offset : Option[L2_ConstIndex] = None,
    var frozen : Boolean = false) extends L2_LeveledKnowledgeAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    if (frozen) out << "frozen ( "
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
    if (frozen) out << " )"
  }

  def getOffset = offset.getOrElse(L2_ConstIndex(Array.fill(target.numDimsGrid)(0)))

  override def progress = ProgressLocation(L3_FieldAccess(target.getProgressedObj(), L3_ActiveSlot, L2_ProgressOption(offset)(_.progress), frozen))
}
