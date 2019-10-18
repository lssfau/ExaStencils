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
import exastencils.prettyprinting._

/// L3_SlotSpecification

trait L3_SlotSpecification extends L3_Node with L3_Progressable with PrettyPrintable {
  override def progress : L4_SlotSpecification
}

/// L3_ActiveSlot

case object L3_ActiveSlot extends L3_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "active"
  override def progress = ProgressLocation(L4_ActiveSlot)
}

/// L3_NextSlot

case object L3_NextSlot extends L3_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "next"
  override def progress = ProgressLocation(L4_NextSlot)
}

/// L3_PreviousSlot

case object L3_PreviousSlot extends L3_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "previous"
  override def progress = ProgressLocation(L4_PreviousSlot)
}

/// L3_ConstantSlot

case class L3_ConstantSlot(slot : Long) extends L3_SlotSpecification {
  override def prettyprint(out : PpStream) = out << slot
  override def progress = ProgressLocation(L4_ConstantSlot(slot))
}

/// L3_AdvanceSlot

case class L3_AdvanceSlot(var access : L3_Access) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << access
  override def progress = ProgressLocation(L4_AdvanceSlot(access.progress))
}
