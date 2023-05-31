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

package exastencils.parallelization.api.mpi

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output

/// MPI_GeneratedTag

case class MPI_GeneratedTag(var from : IR_Expression, var to : IR_Expression, var dirOfSend : IR_Expression, var concurrencyId : Int) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype

  def expand() : Output[IR_Expression] = {
    // ("((unsigned int)" ~ from ~ " << 20)") + ("((unsigned int)(" ~ to ~ ") << 10)") + concurrencyId
    //CastExpression(SpecialDatatype("unsigned int"), from << IntegerConstant(20)) + CastExpression(SpecialDatatype("unsigned int"), to << IntegerConstant(10)) + concurrencyId
    (IR_Cast(IR_SpecialDatatype("unsigned int"), concurrencyId << IR_IntegerConstant(31 - 6))
      + IR_Cast(IR_SpecialDatatype("unsigned int"), from << IR_IntegerConstant(31 - 16))
      + IR_Cast(IR_SpecialDatatype("unsigned int"), to << IR_IntegerConstant(31 - 26))
      + dirOfSend)
  }
}
