package exastencils.performance

import exastencils.config._

object PlatformUtils {
  def numOmpPerNode : Int = Knowledge.omp_numThreads
  def numMpiPerNode : Int = Knowledge.mpi_numThreads / Platform.hw_numNodes
  def numThreadsPerNode : Int = numOmpPerNode * numMpiPerNode

  def cachePerNode : Int = Platform.hw_cacheSize * Platform.hw_numCaches // TODO: less threads than caches

  def cacheSizePerThread : Int = cachePerNode / numThreadsPerNode

}