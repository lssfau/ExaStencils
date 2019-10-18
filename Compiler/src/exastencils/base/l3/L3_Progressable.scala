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

package exastencils.base.l3

import exastencils.base.l4.L4_Node

/// L3_Progressable

trait L3_Progressable {
  def progress : L4_Node
}

/// L3_ProgressOption

object L3_ProgressOption {
  def apply[L3_Type <: L3_Progressable, L4_Type <: L4_Node](toProgress : Option[L3_Type])(progFct : (L3_Type => L4_Type)) : Option[L4_Type] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
