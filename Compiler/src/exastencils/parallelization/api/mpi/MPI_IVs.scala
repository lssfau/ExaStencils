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

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge

abstract class MPI_IV extends IR_UnduplicatedVariable {
  // default value is not applicable since mpi iv will be initialized in a separate routine
  override def resolveDefValue() = None

  def initialization : ListBuffer[IR_Statement]
}

/// IR_IV_MpiRank

case object MPI_IV_MpiRank extends MPI_IV {
  exastencils.core.Duplicate.registerConstant(this)

  override def resolveName() = "mpiRank"
  override def resolveDatatype() = IR_IntegerDatatype

  override def initialization : ListBuffer[IR_Statement] = ListBuffer(
    IR_FunctionCall(IR_ExternalFunctionReference("MPI_Comm_rank"), MPI_IV_MpiComm, IR_AddressOf(MPI_IV_MpiRank)),
    IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::srand"), MPI_IV_MpiRank)))
}

/// IR_IV_MpiSize

case object MPI_IV_MpiSize extends MPI_IV {
  exastencils.core.Duplicate.registerConstant(this)

  override def resolveName() = "mpiSize"
  override def resolveDatatype() = IR_IntegerDatatype

  override def initialization : ListBuffer[IR_Statement] = ListBuffer(
    IR_FunctionCall(IR_ExternalFunctionReference("MPI_Comm_size"), MPI_IV_MpiComm, IR_AddressOf(MPI_IV_MpiSize)))
}

/// IR_IV_MpiComm

case object MPI_IV_MpiComm extends MPI_IV {
  exastencils.core.Duplicate.registerConstant(this)

  override def resolveName() = "mpiCommunicator"
  override def resolveDatatype() = IR_SpecialDatatype("MPI_Comm")

  override def initialization : ListBuffer[IR_Statement] = ListBuffer(
    IR_Assignment(MPI_IV_MpiComm, IR_VariableAccess(Knowledge.mpi_defaultCommunicator, MPI_IV_MpiComm.datatype)))
}

/// MPI_IsRootProc

object MPI_IsRootProc {
  def apply() = 0 EqEq MPI_IV_MpiRank
}
