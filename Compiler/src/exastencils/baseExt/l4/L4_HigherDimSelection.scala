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

package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.l4._
import exastencils.prettyprinting.PpStream

/// L4_HigherDimSelection

case class L4_HigherDimSelection(var base : L4_Expression, var index : L4_ConstIndex) extends L4_Expression {
  override def prettyprint(out : PpStream) = out << base << index.map('[' + _.toString + ']').mkString("")
  override def progress : IR_HighDimAccess = ProgressLocation(IR_HighDimAccess(base.progress, index.progress))
}
