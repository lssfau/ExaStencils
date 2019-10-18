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

package exastencils.solver.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.field.l3._

/// L3_GenerateSmootherHint

object L3_GenerateSmootherHint {
  def apply(loopBase : Option[L3_Access], solveFor : Option[List[L3_Access]]) = new L3_GenerateSmootherHint(loopBase, solveFor.getOrElse(List()).to[ListBuffer])
}

case class L3_GenerateSmootherHint(var loopBase : Option[L3_Access], var solveFor : ListBuffer[L3_Access]) extends L3_Node {
  loopBase = loopBase.map(access => {
    val unresolved = access.asInstanceOf[L3_UnresolvedAccess]
    L3_FutureFieldAccess(unresolved.name, Knowledge.maxLevel/*FIXME*/ , unresolved.slot.getOrElse(L3_ActiveSlot), unresolved.offset)
  })
  solveFor.transform(access => {
    val unresolved = access.asInstanceOf[L3_UnresolvedAccess]
    L3_FutureFieldAccess(unresolved.name, Knowledge.maxLevel/*FIXME*/ , unresolved.slot.getOrElse(L3_ActiveSlot), unresolved.offset)
  })
}
