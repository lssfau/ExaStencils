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

package exastencils.base.ir

import exastencils.core.Duplicate
import exastencils.optimization.ir.IR_GeneralSimplifyWrapper
import exastencils.prettyprinting._

/// IR_Equation

case class IR_Equation(var lhs : IR_Expression, var rhs : IR_Expression) extends IR_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << lhs << " == " << rhs

  def asZeroEquation() : IR_Expression = {
    val zeroEq : IR_Expression = Duplicate(lhs - rhs)
    IR_GeneralSimplifyWrapper.process(zeroEq)
  }
}
