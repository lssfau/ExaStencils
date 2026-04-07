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

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.solver.l3.L3_LocalSolve

object L3_IntroduceSlots extends DefaultStrategy("Add slots to fields and field accesses") {
  def getDistance(slot : L3_SlotSpecification) = {
    slot match {
      case L3_ActiveSlot      => 0
      case L3_PreviousSlot    => -1
      case L3_NextSlot        => 1
      case L3_ConstantSlot(i) => i.toInt
    }
  }

  this += new Transformation("Introduce slots", {
    case access : L3_FieldAccess if access.slot != L3_ActiveSlot =>
      // determine distance to active slot
      val dist = getDistance(access.slot)

      // adapt field's number of slots
      access.target.numSlots = math.max(access.target.numSlots, 1 + math.abs(dist))

      access

    case solve : L3_LocalSolve if solve.jacobiType =>
      var ret = ListBuffer[L3_Statement](solve)

      // update slot number of a fields involved and add advance statements
      for (access <- solve.unknowns.map(_.asInstanceOf[L3_FieldAccess])) {
        access.target.numSlots = math.max(access.target.numSlots, math.abs(2))
      }

      for (access <- solve.unknowns.map(_.asInstanceOf[L3_FieldAccess]).groupBy(_.target.codeName).map(_._2.head)) {
        ret += L3_AdvanceSlot(access)
      }

      ret

    // check assignments to fields as well
    case assignment @ L3_Assignment(lhs : L3_FieldAccess, rhs, _, cond) =>
      if (cond.isDefined) Logger.warn("Found potentially slotted assignment with condition - resolution is currently unsupported")

      // check if the same field is accessed in the rhs of the assignment
      if (cond.isEmpty && StateManager.findFirst({ access : L3_FieldAccess => access.target == lhs.target && access.slot == lhs.slot }, L3_ExpressionStatement(rhs)).isDefined)
        Logger.warn("Found assignment that potentially requires to be slotted")

      assignment
  })
}
