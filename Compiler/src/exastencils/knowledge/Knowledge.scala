package exastencils.knowledge

import exastencils.core._
import exastencils.knowledge._
import exastencils.spl.Configuration
import harald_dep.dsl.DomainKnowledge
import exastencils.util._

object Knowledge {
  // TODO: rename and move to hw knowledge?
  var targetCompiler : String = "MSVC"

  // === Level 1 ===  
  var dimensionality : Int = 3 // dimensionality of the problem

  // TODO: check if these parameters will be necessary or can be implicitly assumed once an appropriate field collection is in place
  var maxLevel : Int = 6 // the finest level
  var numLevels : Int = maxLevel + 1 // the number of levels -> this assumes that the cycle descents to the coarsest level

  // --- Domain Decomposition ---

  // specifies if fragments within one block should be aggregated 
  // TODO: sanity check if compatible with chosen smoother
  var domain_summarizeBlocks : Boolean = true // [true|false]
  var domain_canHaveLocalNeighs : Boolean = true
  var domain_canHaveRemoteNeighs : Boolean = true

  // number of blocks per dimension - one block will usually be mapped to one MPI thread
  var domain_numBlocks_x : Int = 3 // [0-inf]
  var domain_numBlocks_y : Int = 3 // [0-inf]
  var domain_numBlocks_z : Int = 3 // [0-inf]
  def domain_numBlocks : Int = {
    domain_numBlocks_x *
      (if (dimensionality > 1) domain_numBlocks_y else 1) *
      (if (dimensionality > 2) domain_numBlocks_z else 1)
  }

  // number of fragments in each block per dimension - this will usually be one or represent the number of OMP threads per dimension
  var domain_numFragsPerBlock_x : Int = 3 // [0-inf]
  var domain_numFragsPerBlock_y : Int = 3 // [0-inf]
  var domain_numFragsPerBlock_z : Int = 3 // [0-inf]
  def domain_numFragsPerBlock : Int = {
    domain_numFragsPerBlock_x *
      (if (dimensionality > 1) domain_numFragsPerBlock_y else 1) *
      (if (dimensionality > 2) domain_numFragsPerBlock_z else 1)
  }
  def domain_numFragsPerBlockPerDim(index : Int) : Int = {
    Array(domain_numFragsPerBlock_x, domain_numFragsPerBlock_y, domain_numFragsPerBlock_z)(index)
  }

  // the total number of fragments per dimension
  var domain_numFragsTotal_x : Int = domain_numFragsPerBlock_x * domain_numBlocks_x
  var domain_numFragsTotal_y : Int = domain_numFragsPerBlock_y * domain_numBlocks_y
  var domain_numFragsTotal_z : Int = domain_numFragsPerBlock_z * domain_numBlocks_z
  def domain_numFragsTotal : Int = {
    domain_numFragsTotal_x *
      (if (dimensionality > 1) domain_numFragsTotal_y else 1) *
      (if (dimensionality > 2) domain_numFragsTotal_z else 1)
  }

  // the length of each fragment per dimension - this will either be one or specify the length in unit-fragments, i.e. the number of aggregated fragments per dimension
  var domain_fragLength_x : Int = 1
  var domain_fragLength_y : Int = 1
  var domain_fragLength_z : Int = 1
  def domain_fragLength : Int = {
    domain_fragLength_x *
      (if (dimensionality > 1) domain_fragLength_y else 1) *
      (if (dimensionality > 2) domain_fragLength_z else 1)
  }
  def domain_fragLengthPerDim(index : Int) : Int = {
    Array(domain_fragLength_x, domain_fragLength_y, domain_fragLength_z)(index)
  }

  // === Level 2 ===

  // === Level 3 ===

  // === Level 4 ===

  // === Post Level 4 ===

  // --- Data Structures ---

  // --- OpenMP/Hybrid Parallelization ---
  var useOMP : Boolean = true // [true|false]
  var useMPI : Boolean = true // [true|false]
  var omp_numThreads : Int = 1
  var omp_version : Double = 2.0
  var omp_useCollapse : Boolean = true // [true|false]
  var omp_minWorkItemsPerThread : Int = 256 // [1-inf]

  // --- Communication ---
  var comm_strategyFragment : Int = 6 // [6|26]
  var comm_useMPIDatatypes : Boolean = false // [true|false]
  var comm_useLoopsOverNeighbors : Boolean = true // [true|false]

  def update(configuration : Configuration) : Unit = {
    // NOTE: it is required to call update at least once

    useOMP = (domain_summarizeBlocks && domain_fragLength != 1) || domain_numFragsPerBlock != 1
    useMPI = (domain_numBlocks != 1)

    numLevels = maxLevel + 1

    if (domain_summarizeBlocks) {
      // FIXME: move to transformation
      domain_fragLength_x = domain_numFragsPerBlock_x
      domain_fragLength_y = domain_numFragsPerBlock_y
      domain_fragLength_z = domain_numFragsPerBlock_z

      domain_numFragsPerBlock_x = 1
      domain_numFragsPerBlock_y = 1
      domain_numFragsPerBlock_z = 1
    }

    domain_numFragsTotal_x = domain_numFragsPerBlock_x * domain_numBlocks_x
    domain_numFragsTotal_y = domain_numFragsPerBlock_y * domain_numBlocks_y
    domain_numFragsTotal_z = domain_numFragsPerBlock_z * domain_numBlocks_z

    domain_canHaveRemoteNeighs = useMPI 
    domain_canHaveLocalNeighs = (domain_numFragsPerBlock > 1)
    
    if ("MSVC" == targetCompiler)
      omp_version = 2.0
    else if ("GCC" == targetCompiler)
      omp_version = 4.0
    else if ("IBMXL" == targetCompiler)
      omp_version = 3.0
    else
      Logger.error("Unsupported target compiler")

    if (useOMP)
      omp_numThreads = if (domain_summarizeBlocks) domain_fragLength else domain_numFragsPerBlock
  }
}

object DimArray {
  def apply() : Array[Int] = { (0 until Knowledge.dimensionality).toArray }
}
