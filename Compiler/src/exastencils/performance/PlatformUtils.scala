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