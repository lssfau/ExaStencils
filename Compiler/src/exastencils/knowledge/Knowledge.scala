package exastencils.knowledge

import exastencils.core._
import exastencils.spl._

object Knowledge {
  // TODO: rename and move to hw knowledge?
  var targetCompiler : String = "MSVC" // the target compiler; may atm be 'MSVC', 'IBMXL' and 'GCC'
  var targetCompilerVersion : Int = 0 // major version of the target compiler
  var targetCompilerVersionMinor : Int = 0 // minor version of the target compiler

  var simd_instructionSet : String = "AVX" // currently allowed: "SSE3", "AVX", "AVX2"
  var simd_vectorSize : Int = 4 // number of vector elements for SIMD instructions (currently only double precision)

  var useFasterExpand : Boolean = true

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

  // --- Compiler Capabilities ---
  var supports_initializerList = false // indicates if the compiler supports initializer lists (e.g. for std::min)

  // --- Data Structures ---
  var data_initAllFieldsWithZero : Boolean = true // specifies if all data points in all fields on all levels should initially be set zero (before the l4 initField functions are applied)
  var data_useFieldNamesAsIdx : Boolean = true // specifies if generated data field names should hold the clear text field identifier

  var data_addPrePadding : Boolean = true // specifies if additional padding at the beginning of the field is required (e.g. to ensure correct alignment for SIMD accesses)

  // --- OpenMP and MPI Parallelization ---
  var comm_strategyFragment : Int = 6 // [6|26] // specifies if communication is only performed along coordinate axis or to all neighbors
  var comm_useFragmentLoopsForEachOp : Boolean = true // [true|false] // specifies if comm ops (buffer copy, send/ recv, wait) should each be aggregated and handled in distinct fragment loops 

  var comm_useLevelIndependentFcts : Boolean = false

  // TODO: check in how far the following parameters can be adapted by the SPL
  var comm_sepDataByFragment : Boolean = true // specifies if communication variables that could be fragment specific are handled separately
  var comm_sepDataByDomain : Boolean = false // specifies if communication variables that could be domain specific are handled separately
  var comm_sepDataByField : Boolean = false // specifies if communication variables that could be field specific are handled separately
  var comm_sepDataByLevel : Boolean = false // specifies if communication variables that could be level specific are handled separately
  var comm_sepDataByNeighbor : Boolean = true // specifies if communication variables that could be neighbor specific are handled separately

  var comm_useFragmentArrays : Boolean = true // specifies if fragment specific variables are summarized in array form
  var comm_useDomainArrays : Boolean = true // specifies if domain specific variables are summarized in array form
  var comm_useFieldArrays : Boolean = false // specifies if domain field variables are summarized in array form
  var comm_useLevelArrays : Boolean = false // specifies if domain level variables are summarized in array form
  var comm_useNeighborArrays : Boolean = true // specifies if neighbor specific variables are summarized in array form

  // --- OpenMP Parallelization ---
  var useOMP : Boolean = true // [true|false]
  var omp_numThreads : Int = 1 // the number of omp threads to be used; may be incorporated in omp pragmas
  var omp_version : Double = 2.0 // the maximum version of omp supported by the chosen compiler
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

  // --- Polyhedron Optimization ---
  var poly_usePolyOpt : Boolean = false // [true|false]
  var poly_tileSize_x : Int = 1000000000 // 32-inf // TODO: Alex
  var poly_tileSize_y : Int = 1000000000 // 16-inf // TODO: Alex
  var poly_tileSize_z : Int = 1000000000 // 16-inf // TODO: Alex
  var poly_tileSize_w : Int = 1000000000 // 16-inf // TODO: Alex
  var poly_tileOuterLoop : Boolean = false // [true|false] // specify separately if the outermost loop should be tiled

  // --- Other Optimizations ---
  var opt_useAddressPrecalc : Boolean = false // [true|false]
  var opt_vectorize : Boolean = false // [true|false]
  var opt_unroll : Int = 1 // 1-8
  var opt_unroll_interleave : Boolean = true // [true|false]

  /// BEGIN HACK config options for generating L4 DSL file
  /// Student project - Kelvin
  var kelvin : Boolean = false // NOTE: currently only works for 2D
  var numSamples : Int = 10 // only required for kelvin
  var numHaloFrags : Int = 2 // only required for kelvin

  /// SPL connected
  var smoother : String = "Jac" // Jac | GS | RBGS
  var cgs : String = "CG" // CG
  var numPre : Int = 3 // has to be divisible by 2 for Jac if useSlotsForJac or useSlotVariables are disabled
  var numPost : Int = 3 // has to be divisible by 2 for Jac if useSlotsForJac or useSlotVariables are disabled
  var omega : Double = (if ("Jac" == smoother) 0.8 else 1.0)
  var testStencilStencil : Boolean = false
  var genStencilFields : Boolean = false
  var testCommCompOverlap : Boolean = false
  var genRBSetsWithConditions : Boolean = true
  var useSlotsForJac : Boolean = true
  var useSlotVariables : Boolean = true
  var testTempBlocking : Boolean = false

  /// functionality test
  var testBC : Boolean = true
  var testExtFields : Boolean = false
  var omegaViaGlobals : Boolean = false
  var genSetableStencil : Boolean = false
  var useVecFields : Boolean = false // attempts to solve Poisson's equation for (numVecDims)D vectors; atm all components are solved independently
  var numVecDims = (if (useVecFields) 2 else 1)
  var testFragLoops = true
  var testDomainEmbedding = false

  /// optional features  
  var printFieldAtEnd : Boolean = false
  var initSolWithRand : Boolean = true

  // Student project - Oleg
  var testNewTimers : Boolean = false // this is to enable the usage of some new, currently highly experimental, timer classes; requires that the timer classes are provided
  var genTimersPerFunction : Boolean = false
  var genTimersPerLevel : Boolean = false
  var genTimersForComm : Boolean = false
  var genCommTimersPerLevel : Boolean = false
  /// END HACK

  def update(configuration : Configuration = new Configuration) : Unit = {
    // NOTE: it is required to call update at least once

    Constraints.updateValue(useOMP, (domain_summarizeBlocks && domain_fragLength != 1) || domain_numFragsPerBlock != 1)
    Constraints.updateValue(useMPI, (domain_numBlocks != 1))

    Constraints.updateValue(numLevels, maxLevel + 1)

    if (domain_summarizeBlocks) {
      Constraints.updateValue(domain_fragLength_x, domain_numFragsPerBlock_x)
      Constraints.updateValue(domain_fragLength_y, domain_numFragsPerBlock_y)
      Constraints.updateValue(domain_fragLength_z, domain_numFragsPerBlock_z)

      Constraints.updateValue(domain_numFragsPerBlock_x, 1)
      Constraints.updateValue(domain_numFragsPerBlock_y, 1)
      Constraints.updateValue(domain_numFragsPerBlock_z, 1)
    }

    Constraints.updateValue(domain_numFragsTotal_x, domain_numFragsPerBlock_x * domain_numBlocks_x)
    Constraints.updateValue(domain_numFragsTotal_y, domain_numFragsPerBlock_y * domain_numBlocks_y)
    Constraints.updateValue(domain_numFragsTotal_z, domain_numFragsPerBlock_z * domain_numBlocks_z)

    Constraints.updateValue(domain_canHaveRemoteNeighs, useMPI)
    Constraints.updateValue(domain_canHaveLocalNeighs, (domain_numFragsPerBlock > 1))

    if ("MSVC" == targetCompiler) {
      Constraints.updateValue(omp_version, 2.0)
      Constraints.updateValue(supports_initializerList, targetCompilerVersion >= 18)
    } else if ("GCC" == targetCompiler) {
      Constraints.updateValue(omp_version, 4.0)
      Constraints.updateValue(supports_initializerList, targetCompilerVersion > 4 || (targetCompilerVersion == 4 && targetCompilerVersionMinor >= 5))
    } else if ("IBMXL" == targetCompiler) {
      Constraints.updateValue(omp_version, 3.0)
      Constraints.updateValue(omp_requiresCriticalSections, false)
      Constraints.updateValue(supports_initializerList, false) // TODO: does it support initializer lists? since which version?
    } else
      Logger.error("Unsupported target compiler")

    if (useOMP) {
      if (domain_summarizeBlocks) Constraints.updateValue(omp_numThreads, domain_fragLength) else Constraints.updateValue(omp_numThreads, domain_numFragsPerBlock)
      Constraints.updateValue(omp_parallelizeLoopOverFragments, !domain_summarizeBlocks)
      Constraints.updateValue(omp_parallelizeLoopOverDimensions, domain_summarizeBlocks)
    }

    simd_instructionSet match {
      case "SSE3"         => Constraints.updateValue(simd_vectorSize, 2)
      case "AVX" | "AVX2" => Constraints.updateValue(simd_vectorSize, 4)
    }

    // update constraints
    Constraints.condEnsureValue(numPre, numPre - (numPre % 2), "Jac" == smoother && !useSlotsForJac && !useSlotVariables,
      "Number of pre-smoothing steps has to be divisible by 2")
    Constraints.condEnsureValue(numPost, numPost - (numPost % 2), "Jac" == smoother && !useSlotsForJac && !useSlotVariables,
      "Number of post-smoothing steps has to be divisible by 2")

    if ("Jac" == smoother) Constraints.updateValue(omega, 0.8) else Constraints.updateValue(omega, 1.0)

    Constraints.condEnsureValue(testStencilStencil, true, kelvin, "required by kelvin")
    Constraints.condEnsureValue(genStencilFields, true, kelvin, "required by kelvin")
    Constraints.condEnsureValue(printFieldAtEnd, true, kelvin, "required by kelvin")
    Constraints.condEnsureValue(testBC, false, kelvin, "not compatible with kelvin")
    Constraints.condEnsureValue(genSetableStencil, false, kelvin, "not compatible with kelvin")
    Constraints.condEnsureValue(useVecFields, false, kelvin, "not compatible with kelvin")
    Constraints.condEnsureValue(testDomainEmbedding, false, kelvin, "not compatible with kelvin")
    Constraints.condEnsureValue(initSolWithRand, false, kelvin, "not compatible with kelvin")

    Constraints.condEnsureValue(testCommCompOverlap, false, 26 != comm_strategyFragment, "invalid comm_strategyFragment")

    Constraints.condWarn("RBGS" == smoother && !genRBSetsWithConditions,
      s"Currently NOT using genRBSetsWithConditions leads to a color mismatch at primitive boundaries and thus to a reduced convergence rate")

    Constraints.condEnsureValue(useSlotVariables, false, !useSlotsForJac, "invalid if not using useSlotsForJac")
    Constraints.condEnsureValue(testTempBlocking, false, numPre != numPost, "numPre and numPost have to be equal")
    Constraints.condWarn("GS" != smoother && testTempBlocking, "testTempBlocking currently only works with GS")

    Constraints.condEnsureValue(testBC, false, 2 != dimensionality, "testBC is only valid for 2D problems")
    Constraints.condEnsureValue(initSolWithRand, true, !testBC, "initial solution of zero corresponds to the exact solution if testBC is false")
    Constraints.condEnsureValue(initSolWithRand, false, testBC, "testBC requires initial solution of zero")

    if (useVecFields) Constraints.updateValue(numVecDims, 2) else Constraints.updateValue(numVecDims, 1)

    Constraints.condEnsureValue(genTimersPerFunction, false, !testNewTimers, "requires testNewTimers to be activated")
    Constraints.condEnsureValue(genTimersPerLevel, false, !testNewTimers, "requires testNewTimers to be activated")
    Constraints.condEnsureValue(genTimersForComm, false, !testNewTimers, "requires testNewTimers to be activated")
    Constraints.condEnsureValue(genCommTimersPerLevel, false, !testNewTimers, "requires testNewTimers to be activated")

    Constraints.condEnsureValue(genTimersForComm, false, testCommCompOverlap, "timers for overlapping communication are not yet supported")
    Constraints.condEnsureValue(genCommTimersPerLevel, false, !genTimersForComm, "requires genTimersForComm to be activated")

    Constraints.condEnsureValue(comm_useLevelIndependentFcts, false, testBC, "level independent communication functions are not compatible with non-trivial boundary conditions")
  }
}
