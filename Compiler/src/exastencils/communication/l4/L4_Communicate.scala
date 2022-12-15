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

package exastencils.communication.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.communication.ir._
import exastencils.core.Duplicate
import exastencils.fieldlike.l4.L4_FieldLikeAccess
import exastencils.operator.l4.L4_StencilFieldAccess
import exastencils.prettyprinting._

/// L4_Communicate

object L4_Communicate {
  def apply(field : L4_Access, op : String, targets : List[L4_CommunicateTarget], condition : Option[L4_Expression], direction : Option[String]) =
    new L4_Communicate(field, op, targets.to[ListBuffer], condition, direction)
}

case class L4_Communicate(
    var field : L4_Access,
    var op : String,
    var targets : ListBuffer[L4_CommunicateTarget],
    var condition : Option[L4_Expression],
    var direction : Option[String]) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    if ("both" != op) out << op + ' '
    out << "communicate " <<< (targets, " ") << (if (targets.isEmpty) "" else " of ") << field
    if (condition.isDefined) out << " where " << condition.get
    if (direction.isDefined) out << " " << direction.get
  }

  override def progress : IR_Communicate = ProgressLocation {
    // TODO: extract to strategy replacing stencil field accesses with corresponding field accesses
    // FIXME: honor component accesses
    val (progressedField, progressedSlot) = Duplicate(field match {
      case f : L4_FieldLikeAccess         => (f.progress.field, f.progress.slot)
      case sf : L4_StencilFieldAccess => (sf.target.getProgressedObj().field, L4_FieldLikeAccess.resolveSlot(sf.target.getProgressedObj().field, sf.slot))
    })
    val progressedTargets : ListBuffer[IR_CommunicateTarget] = ListBuffer()

    if (targets.isEmpty)
      progressedTargets += L4_CommunicateTarget("all", None, None).progress
    else
      for (t <- targets) progressedTargets += t.progress

    IR_Communicate(progressedField, progressedSlot, op, progressedTargets, L4_ProgressOption(condition)(_.progress), direction.getOrElse(""))
  }
}

/// L4_CommunicateTarget

case class L4_CommunicateTarget(
    var target : String,
    var begin : Option[L4_ConstIndex],
    var end : Option[L4_ConstIndex]) extends L4_Node with L4_Progressable with PrettyPrintable {

  override def prettyprint(out : PpStream) = {
    out << target
    if (begin.isDefined) out << ' ' << begin.get
    if (end.isDefined) out << " to " << end.get
  }

  // FIXME: remove .toExpressionIndex
  override def progress = ProgressLocation(IR_CommunicateTarget(target, L4_ProgressOption(begin)(_.progress.toExpressionIndex), L4_ProgressOption(end)(_.progress.toExpressionIndex)))
}
