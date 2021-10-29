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

package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.communication.ir.IR_CommunicationFunctions
import exastencils.config._
import exastencils.core._
import exastencils.domain.ir.IR_DomainFunctions
import exastencils.field.ir.IR_InitFieldsWithZero
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.parallelization.api.cuda.CUDA_KernelFunctions
import exastencils.timing.ir.IR_TimerFunctions

/// IR_UserFunctions

object IR_UserFunctions extends ObjectWithState {
  def defBaseName = "User/User"
  def defHeader = defBaseName + ".h"

  // buffer looked up reference to reduce execution time
  var selfRef : Option[IR_UserFunctions] = None

  override def clear() = { selfRef = None }

  // looks itself up starting from the current root
  def get = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[IR_UserFunctions]()
    selfRef.get
  }
}

case class IR_UserFunctions() extends IR_FunctionCollection(IR_UserFunctions.defBaseName,
  ListBuffer("cmath", "algorithm", "iostream"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer(IR_GlobalCollection.defHeader, IR_TimerFunctions.defHeader, IR_CommunicationFunctions.defHeader, IR_DomainFunctions.defHeader)) {

  // add conditional dependencies - TODO: move to respective packages
  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.cuda_enabled) {
    externalDependencies += "cuda.h"
    externalDependencies += "cuda_runtime.h"

    internalDependencies += CUDA_KernelFunctions.defHeader
  }
  if (Knowledge.opt_vectorize) {
    val header = Platform.simd_header
    if (header != null)
      externalDependencies += header
    val mathLibHeader = Platform.simd_mathLibHeader
    if (mathLibHeader != null)
      externalDependencies ++= mathLibHeader
  }
  // TODO: move to fields package
  if (Knowledge.data_initAllFieldsWithZero)
    functions += IR_InitFieldsWithZero()

  override def toString : String = "IR_UserFunctions(" + baseName + ", " + externalDependencies + ", " + internalDependencies + ", " + functions + ")"
}
