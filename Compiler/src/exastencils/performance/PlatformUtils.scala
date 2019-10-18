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

package exastencils.performance

import exastencils.config.{ Platform, _ }

object PlatformUtils {
  def numOmpPerNode : Int = Knowledge.omp_numThreads
  def numMpiPerNode : Int = Knowledge.mpi_numThreads / Platform.hw_numNodes
  def numThreadsPerNode : Int = numOmpPerNode * numMpiPerNode
  def numThreadsPerCore : Double = // could be a fraction
    numThreadsPerNode / (Platform.hw_cpu_numCoresPerCPU * Platform.hw_cpu_numCPUs)

  def cacheSizePerThread : Double = {
    val usableCachePerNode = Platform.hw_cacheSize * Math.min(numThreadsPerNode, Platform.hw_numCaches)
    usableCachePerNode / numThreadsPerNode
  }

  def cpu_bandwidthPerThread : Double = {
    var ret = Platform.hw_cpu_bandwidth
    ret /= Math.max(numThreadsPerNode, Platform.hw_cpu_numCPUs) // honor number of threads and cases with less threads than sockets
    ret
  }

  def gpu_bandwidthPerMpi : Double = {
    var ret = Platform.hw_gpu_bandwidth
    ret /= PlatformUtils.numMpiPerNode// incorporate device sharing
    ret
  }

  def cpu_opsPerThread : Double = {
    var ret = Platform.hw_cpu_frequency
    ret /= Math.min(1.0, numThreadsPerCore) // honor under- and oversubscription
    ret *= Platform.simd_vectorSize // assume perfect vectorization
    ret
  }

  def gpu_opsPerMpi(maxIterations : Double) : Double = {
    var ret = Platform.hw_gpu_numDevices * Platform.hw_gpu_frequency
    val coresPerMpi = Platform.hw_gpu_numCores / PlatformUtils.numMpiPerNode// incorporate device sharing
    ret *= Math.min(maxIterations, coresPerMpi)// assumes perfect utilization as far as possible
    ret
  }
}