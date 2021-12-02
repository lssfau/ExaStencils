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
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.prettyprinting.PpStream

/// MPI_Barrier
case object MPI_Barrier extends MPI_Statement {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Barrier(" << MPI_IV_MpiComm << ");"
}

/// MPI_Sequential

object MPI_Sequential {
  def apply(body : IR_Statement*) = new MPI_Sequential(body.to[ListBuffer])
}

case class MPI_Sequential(var body : ListBuffer[IR_Statement]) extends IR_Statement with IR_Expandable {
  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, "curRank", 0),
      IR_Lower("curRank", Knowledge.mpi_numThreads),
      IR_PreIncrement("curRank"),
      ListBuffer[IR_Statement](
        MPI_Barrier,
        IR_IfCondition(IR_EqEq(MPI_IV_MpiRank, "curRank"), body)))
  }
}
