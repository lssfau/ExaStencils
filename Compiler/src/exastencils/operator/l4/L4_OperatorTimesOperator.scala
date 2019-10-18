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

package exastencils.operator.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Expression
import exastencils.operator.ir.IR_OperatorTimesOperator
import exastencils.prettyprinting.PpStream

/// L4_OperatorTimesOperator

case class L4_OperatorTimesOperator(var left : L4_OperatorAccess, var right : L4_OperatorAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << left << " * " << right
  override def progress = ProgressLocation(IR_OperatorTimesOperator(left.progress, right.progress))
}
