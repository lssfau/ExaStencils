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

import exastencils.prettyprinting.PpStream

/// MPI_Init

case object MPI_Init extends MPI_Statement {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Init(&argc, &argv);"
}

/// MPI_Finalize

case object MPI_Finalize extends MPI_Statement {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Finalize();"
}
