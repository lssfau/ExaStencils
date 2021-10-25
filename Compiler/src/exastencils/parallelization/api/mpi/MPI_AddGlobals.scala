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
import exastencils.datastructures._
import exastencils.globals.ir.IR_GlobalCollection

object MPI_AddGlobals extends NoTraversalStrategy("Extend globals for MPI") {
  override def doWork() : Unit = {
    val globals = IR_GlobalCollection.get
    val initFunc = globals.functions.find(_.name == "initGlobals").get.asInstanceOf[IR_Function]

    initFunc.body ++= MPI_IV_MpiComm.initialization
    initFunc.body ++= MPI_IV_MpiRank.initialization
    initFunc.body ++= MPI_IV_MpiSize.initialization
  }
}
