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

import exastencils.base.ir.IR_IntegerConstant
import exastencils.datastructures._

/// MPI_RemoveMPI

// TODO: prevent generation in the first place
object MPI_RemoveMPI extends DefaultStrategy("Remove references to mpi functions and variables") {
  this += new Transformation("Clean", {
    // TODO: think about replacing reduce, gather, etc. with copy operations
    case _ : MPI_Statement => List()
    case MPI_IV_MpiRank    => IR_IntegerConstant(0)
  })
}
