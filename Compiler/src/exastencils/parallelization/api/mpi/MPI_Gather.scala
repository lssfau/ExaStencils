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
import exastencils.prettyprinting.PpStream

/// MPI_Gather

object MPI_Gather {
  def apply(buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression) =
    new MPI_Gather("MPI_IN_PLACE", buf, datatype, count)
}

case class MPI_Gather(var sendbuf : IR_Expression, var recvbuf : IR_Expression, var datatype : IR_Datatype, var count : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    (out << "MPI_Gather("
      << sendbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", "
      << recvbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", "
      << 0 << ", " << MPI_IV_MpiComm << ");")
  }
}
