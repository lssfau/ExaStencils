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

import exastencils.base.l4.L4_Assignment
import exastencils.baseExt.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.fieldlike.l4.L4_FieldLikeAccess

object L4_AddLoopsToFieldAssignments extends DefaultStrategy("Add loop statements around field assignments") {
  this += new Transformation("Add loops", {
    case assignment @ L4_Assignment(lhs : L4_FieldLikeAccess, rhs, op, cond) =>
      val loop = L4_LoopOverField(Duplicate(lhs), assignment)
      loop.condition = cond

      // TODO: add real switch
      if (false)
        L4_LoopOverFragments(loop)
      else
        loop
  }, false /* recursion must be switched of due to wrapping mechanism */)
}
