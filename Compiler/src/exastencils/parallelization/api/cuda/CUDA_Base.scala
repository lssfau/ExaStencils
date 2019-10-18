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

package exastencils.parallelization.api.cuda

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

/// CUDA_Init

case object CUDA_Init extends CUDA_DeviceStatement {
  override def prettyprint(out : PpStream) : Unit = {
    if (!Knowledge.experimental_eliminateCudaContext)
      out << "cuInit(0);"
  }
}

/// CUDA_Finalize

case object CUDA_Finalize extends CUDA_DeviceStatement {
  override def prettyprint(out : PpStream) : Unit = {
    // has to be done after all other de-initialization statements
    if (!Knowledge.experimental_eliminateCudaContext)
      out << "cuCtxDestroy(cudaContext);"
  }
}

/// CUDA_DeviceSynchronize

case class CUDA_DeviceSynchronize() extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall("cudaDeviceSynchronize"))
}

/// CUDA_SyncThreads

case class CUDA_SyncThreads() extends CUDA_HostStatement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "__syncthreads();"
  }
}
