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

package exastencils.field.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.field.ir._
import exastencils.fieldlike.l4.L4_FieldLikeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_SlotSpecification

trait L4_SlotSpecification extends L4_Node with PrettyPrintable {}

/// L4_ActiveSlot

case object L4_ActiveSlot extends L4_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "active"
}

/// L4_NextSlot

case object L4_NextSlot extends L4_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "next"
}

/// L4_PreviousSlot

case object L4_PreviousSlot extends L4_SlotSpecification {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << "previous"
}

/// L4_ConstantSlot

case class L4_ConstantSlot(number : Long) extends L4_SlotSpecification {
  override def prettyprint(out : PpStream) = out << number
}

/// L4_AdvanceSlot

case class L4_AdvanceSlot(var field : L4_Access) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    // don't use field's prettyprinter -> slot modifiers are not required in advance statements
    field match {
      case access : L4_FieldLikeAccess =>
        out << "advance " << access.target.name << "@" << access.target.level

      case access : L4_UnresolvedAccess =>
        out << "advance " << field.name
        if (access.level.isDefined) out << "@" << access.level.get

      case access =>
        Logger.warn("Trying to advance slot of something that is not a field: " + access)
        out << "advance " << access
    }
  }

  override def progress = ProgressLocation {
    // TODO: check for unsupported modifiers (offset, etc)

    val f = field match {
      case fAcc : L4_FieldLikeAccess => fAcc.target.getProgressedObj()
      case access                    =>
        Logger.error("Trying to advance slot of something that is not a field: " + access)
    }

    IR_AdvanceSlot(IR_IV_ActiveSlot(f, IR_LoopOverFragments.defIt))
  }
}
