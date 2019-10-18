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

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.core.Duplicate
import exastencils.optimization.l2.L2_GeneralSimplifyWrapper
import exastencils.prettyprinting._

/// L2_Equation

case class L2_Equation(var lhs : L2_Expression, var rhs : L2_Expression) extends L2_Node with PrettyPrintable with L2_Progressable {
  override def prettyprint(out : PpStream) = out << lhs << " == " << rhs
  override def progress = ProgressLocation(L3_Equation(lhs.progress, rhs.progress))

  def asZeroEquation() : L2_Expression = {
    val zeroEq : L2_Expression = Duplicate(lhs - rhs)
    L2_GeneralSimplifyWrapper.process(zeroEq)
  }
}
