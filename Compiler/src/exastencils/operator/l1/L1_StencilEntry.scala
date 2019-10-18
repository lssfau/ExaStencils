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

package exastencils.operator.l1

import exastencils.base.ProgressLocation
import exastencils.base.l1._
import exastencils.operator.l2._
import exastencils.prettyprinting._

/// L1_StencilOffsetEntry

case class L1_StencilEntry(var offset : L1_ConstIndex, var coefficient : L1_Expression) extends L1_Node with L1_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << offset << " => " << coefficient
  override def progress = ProgressLocation(L2_StencilOffsetEntry(offset.progress, coefficient.progress))
  def numDims = offset.length
}
