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

package exastencils.operator.ir

import exastencils.base.ir.IR_Expression
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// IR_OperatorTimesOperator

case class IR_OperatorTimesOperator(var lhs : IR_OperatorAccess, var rhs : IR_OperatorAccess) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(lhs.datatype, rhs.datatype)
  override def prettyprint(out : PpStream) = out << lhs << " * " << rhs
}