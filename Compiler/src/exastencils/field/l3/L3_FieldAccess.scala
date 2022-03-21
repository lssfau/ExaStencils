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

package exastencils.field.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.field.l4._
import exastencils.knowledge.l3._
import exastencils.prettyprinting.PpStream

/// L3_FieldAccess

object L3_FieldAccess {
  def apply(access : L3_FutureFieldAccess) =
    new L3_FieldAccess(L3_FieldCollection.getByIdentifier(access.name, access.level).get, access.slot, access.offset, access.frozen)
  def apply(target : L3_Field) = new L3_FieldAccess(target, L3_ActiveSlot, None)
}

case class L3_FieldAccess(
    var target : L3_Field, // TODO: abstract class
    var slot : L3_SlotSpecification,
    var offset : Option[L3_ConstIndex] = None,
    var frozen : Boolean = false) extends L3_LeveledKnowledgeAccess with L3_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    if (frozen) out << "frozen ( "
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
    if (frozen) out << " )"
  }

  def getOffset = offset.getOrElse(L3_ConstIndex(Array.fill(target.numDimsGrid)(0)))

  override def progress = ProgressLocation(L4_FieldAccess(target.getProgressedObj(), slot.progress, L3_ProgressOption(offset)(_.progress), frozen))
}
