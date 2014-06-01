package exastencils.knowledge

import exastencils.core._
import exastencils.knowledge._
import exastencils.spl.Configuration
import harald_dep.dsl.DomainKnowledge
import exastencils.util._
import exastencils.spl.Configuration

object Knowledge {
  // TODO: rename and move to hw knowledge?
  var targetCompiler : String = "MSVC" // the target compiler; may atm be 'MSVC', 'IBMXL' and 'GCC'

  // === Level 1 ===  
  var dimensionality : Int = 3 // dimensionality of the problem

  // TODO: check if these parameters will be necessary or can be implicitly assumed once an appropriate field collection is in place
  var maxLevel : Int = 6 // the finest level
  var numLevels : Int = maxLevel + 1 // the number of levels -> this assumes that the cycle descents to the coarsest level

  // --- Domain Decomposition ---

  // specifies if fragments within one block should be aggregated 
  // TODO: sanity check if compatible with chosen smoother
  var domain_summarizeBlocks : Boolean = true // [true|false] // if true, fragments inside one block are aggregated into one bigger fragment
  var domain_canHaveLocalNeighs : Boolean = true // specifies if fragments can have local (i.e.\ shared memory) neighbors, i.e.\ if local comm is required
  var domain_canHaveRemoteNeighs : Boolean = true // specifies if fragments can have remote (i.e.\ different mpi rank) neighbors, i.e.\ if mpi comm is required

  // number of blocks per dimension - one block will usually be mapped to one MPI thread
  var domain_numBlocks_x : Int = 3 // 0-inf
  var domain_numBlocks_y : Int = 3 // 0-inf
  var domain_numBlocks_z : Int = 3 // 0-inf
  def domain_numBlocks : Int = {
    domain_numBlocks_x *
      (if (dimensionality > 1) domain_numBlocks_y else 1) *
      (if (dimensionality > 2) domain_numBlocks_z else 1)
  }

  // number of fragments in each block per dimension - this will usually be one or represent the number of OMP threads per dimension
  var domain_numFragsPerBlock_x : Int = 3 // 0-inf
  var domain_numFragsPerBlock_y : Int = 3 // 0-inf
  var domain_numFragsPerBlock_z : Int = 3 // 0-inf
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
  var data_initAllFieldsWithZero : Boolean = true // specifies if all data points in all fields on all levels should initially be set zero (before the l4 initField functions are applied)

  // --- OpenMP and MPI Parallelization ---
  var comm_strategyFragment : Int = 6 // [6|26] // specifies if communication is only performed along coordinate axis or to all neighbors

  // --- OpenMP Parallelization ---
  var useOMP : Boolean = true // [true|false]
  var omp_numThreads : Int = 1 // the number of omp threads to be used; may be incorporated in omp pragmas
  var omp_version : Double = 2.0 // the minimum version of omp supported by the chosen compiler
  var omp_parallelizeLoopOverFragments : Boolean = false // [true|false] // specifies if loops over fragments may be parallelized with omp if marked correspondingly
  var omp_parallelizeLoopOverDimensions : Boolean = true // [true|false] // specifies if loops over dimensions may be parallelized with omp if marked correspondingly
  var omp_useCollapse : Boolean = true // [true|false] // if true the 'collapse' directive may be used in omp for regions; this will only be done if the minimum omp version supports this
  var omp_minWorkItemsPerThread : Int = 256 // [1-inf] // threshold specifying which loops yield enough workload to amortize the omp overhead
  var omp_requiresCriticalSections : Boolean = true // true if the chosen compiler / mpi version requires critical sections to be marked explicitly

  // --- MPI Parallelization ---
  var useMPI : Boolean = true // [true|false]
  var mpi_useCustomDatatypes : Boolean = false // [true|false] // allows to use custom mpi data types when reading from/ writing to fields thus circumventing temp send/ receive buffers
  var mpi_useLoopsWherePossible : Boolean = true // [true|false] // allows to summarize some code blocks into loops in order to shorten the resulting code length
  var mpi_defaultCommunicator : String = "MPI_COMM_WORLD" // sets the initial communicator used by most MPI operations

  def update(configuration : Configuration = new Configuration) : Unit = {
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
    else if ("IBMXL" == targetCompiler) {
      omp_version = 3.0
      omp_requiresCriticalSections = false
    } else
      Logger.error("Unsupported target compiler")

    if (useOMP) {
      omp_numThreads = if (domain_summarizeBlocks) domain_fragLength else domain_numFragsPerBlock
      omp_parallelizeLoopOverFragments = !domain_summarizeBlocks
      omp_parallelizeLoopOverDimensions = domain_summarizeBlocks
    }
  }
}

object DimArray {
  def apply() : Array[Int] = { (0 until Knowledge.dimensionality).toArray }
}
