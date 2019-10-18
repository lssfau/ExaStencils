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

package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.parallelization.api.cuda.CUDA_KernelFunctions

/// IR_CommunicationFunctions

object IR_CommunicationFunctions extends ObjectWithState {
  def defBaseName = "Communication/Communication"
  def defHeader = defBaseName + ".h"

  // buffer looked up reference to reduce execution time
  var selfRef : Option[IR_CommunicationFunctions] = None

  override def clear() = { selfRef = None }

  // looks itself up starting from the current root
  def get = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[IR_CommunicationFunctions]()
    selfRef.get
  }
}

case class IR_CommunicationFunctions() extends IR_FunctionCollection(IR_CommunicationFunctions.defBaseName,
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer(IR_GlobalCollection.defHeader, IR_UserFunctions.defHeader)) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"

  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"

  if (Knowledge.cuda_enabled)
    internalDependencies += CUDA_KernelFunctions.defHeader

  if (Knowledge.opt_vectorize)
    if (Platform.simd_header != null) externalDependencies += Platform.simd_header
}
