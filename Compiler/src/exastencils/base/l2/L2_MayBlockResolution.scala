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

package exastencils.base.l2

import exastencils.core.StateManager

/// L2_MayBlockResolution

object L2_MayBlockResolution {
  // no wrapping since provided statement/ expression is usually subclass of L2_MayBlockResolution
  def isDone(stmt : L2_Statement) = StateManager.findFirst({ mayBlock : L2_MayBlockResolution => !mayBlock.allDone }, stmt).isEmpty
  def isDone(expr : L2_Expression) = StateManager.findFirst({ mayBlock : L2_MayBlockResolution => !mayBlock.allDone }, expr).isEmpty
}

trait L2_MayBlockResolution {
  var allDone = false
}
