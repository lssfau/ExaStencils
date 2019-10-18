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

package exastencils.base.l4

import exastencils.base.ir.IR_Node

/// L4_Progressable

trait L4_Progressable {
  def progress : IR_Node
}

/// L4_ProgressOption

object L4_ProgressOption {
  def apply[L4_Type <: L4_Progressable, IR_Type <: IR_Node](toProgress : Option[L4_Type])(progFct : (L4_Type => IR_Type)) : Option[IR_Type] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
