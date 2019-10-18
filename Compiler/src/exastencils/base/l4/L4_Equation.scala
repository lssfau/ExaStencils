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

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.optimization.l4.L4_GeneralSimplifyWrapper
import exastencils.prettyprinting._

/// L4_Equation

case class L4_Equation(var lhs : L4_Expression, var rhs : L4_Expression) extends L4_Node with PrettyPrintable with L4_Progressable {
  override def prettyprint(out : PpStream) = out << lhs << " == " << rhs
  override def progress = ProgressLocation(IR_Equation(lhs.progress, rhs.progress))

  def asZeroEquation() : L4_Expression = {
    val zeroEq : L4_Expression = Duplicate(lhs - rhs)
    L4_GeneralSimplifyWrapper.process(zeroEq)
  }
}
