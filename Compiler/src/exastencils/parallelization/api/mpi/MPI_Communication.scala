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

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

/// MPI_Receive

case class MPI_Receive(var buffer : IR_Expression, var size : IR_Expression, var datatype : IR_Datatype, var rank : IR_Expression, var tag : IR_Expression, var request : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Irecv(" << buffer << ", " << size << ", " << datatype.prettyprint_mpi << ", " << rank << ", " << tag << ", " << MPI_IV_MpiComm << ", &" << request << ");"
  }
}

/// MPI_Send

case class MPI_Send(var buffer : IR_Expression, var size : IR_Expression, var datatype : IR_Datatype, var rank : IR_Expression, var tag : IR_Expression, var request : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Isend(" << buffer << ", " << size << ", " << datatype.prettyprint_mpi << ", " << rank << ", " << tag << ", " << MPI_IV_MpiComm << ", &" << request << ");"
  }
}

