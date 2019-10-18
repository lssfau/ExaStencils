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
import exastencils.baseExt.ir._

/// IR_IV_MpiRank

case object MPI_IV_MpiRank extends IR_UnduplicatedVariable {
  exastencils.core.Duplicate.registerConstant(this)

  override def resolveName() = "mpiRank"
  override def resolveDatatype() = IR_IntegerDatatype

  // default value is not applicable since mpi rank will be initialized in a separate routine
  override def resolveDefValue() = None
}

/// MPI_IsRootProc

object MPI_IsRootProc {
  def apply() = 0 EqEq MPI_IV_MpiRank
}
