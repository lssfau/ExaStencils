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

import exastencils.base.l3.L3_Node

/// L2_Progressable

trait L2_Progressable {
  def progress : L3_Node
}

/// L2_ProgressOption

object L2_ProgressOption {
  def apply[L2_Type <: L2_Progressable, L3_Type <: L3_Node](toProgress : Option[L2_Type])(progFct : (L2_Type => L3_Type)) : Option[L3_Type] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
