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

package exastencils.baseExt.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_HigherDimSelection
import exastencils.prettyprinting.PpStream

/// L3_HigherDimSelection

case class L3_HigherDimSelection(var base : L3_Expression, var index : L3_ConstIndex) extends L3_Expression {
  override def prettyprint(out : PpStream) = out << base << index.map('[' + _.toString + ']').mkString("")
  override def progress : L4_HigherDimSelection = ProgressLocation(L4_HigherDimSelection(base.progress, index.progress))
}
