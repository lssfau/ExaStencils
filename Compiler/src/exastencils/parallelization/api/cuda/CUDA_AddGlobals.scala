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

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.util.ir.IR_RawPrint

object CUDA_AddGlobals extends NoTraversalStrategy("Extend globals for CUDA") {
  override def doWork() : Unit = {
    val globals = IR_GlobalCollection.get

    val initFunc = globals.functions.find(_.name == "initGlobals").get.asInstanceOf[IR_Function]

    // get device count
    initFunc.body ++= CUDA_DeviceCount.setup()

    // print device info (name)
    if (!Knowledge.testing_enabled)
      initFunc.body ++= CUDA_DeviceProperties.setup()

    // set L1 cache and shared memory configuration for this device
    if (Knowledge.cuda_useSharedMemory)
      initFunc.body += "cudaDeviceSetCacheConfig(cudaFuncCachePreferShared)"
    if (Knowledge.cuda_favorL1CacheOverSharedMemory)
      initFunc.body += "cudaDeviceSetCacheConfig(cudaFuncCachePreferL1)"
  }
}
