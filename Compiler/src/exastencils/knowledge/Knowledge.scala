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
  // TODO: sanity check if compatible with chosen l3tmp_smoother
  var domain_summarizeBlocks : Boolean = true // [true|false] // if true, fragments inside one block are aggregated into one bigger fragment
  var domain_canHaveLocalNeighs : Boolean = true // specifies if fragments can have local (i.e.\ shared memory) neighbors, i.e.\ if local comm is required
  var domain_canHaveRemoteNeighs : Boolean = true // specifies if fragments can have remote (i.e.\ different mpi rank) neighbors, i.e.\ if mpi comm is required

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

  // === Layer 2 ===

  // === Layer 3 ===

  // === Layer 4 ===

  // === Post Layer 4 ===

  // --- Compiler Capabilities ---
  var supports_initializerList = false // indicates if the compiler supports initializer lists (e.g. for std::min)

  // --- Data Structures ---
  var data_initAllFieldsWithZero : Boolean = true // specifies if all data points in all fields on all levels should initially be set zero (before the l4 initField functions are applied)
  var data_useFieldNamesAsIdx : Boolean = true // specifies if generated data field names should hold the clear text field identifier

  var data_addPrePadding : Boolean = false // TODO: currently removed due to problems with level-dependent layouts; specifies if additional padding at the beginning of the field is required (e.g. to ensure correct alignment for SIMD accesses)

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
  /// SPL connected
  var l3tmp_smoother : String = "Jac" // [Jac|GS|RBGS] // the l3tmp_smoother to be generated
  var l3tmp_cgs : String = "CG" // [CG] // the coarse grid solver to be generated
  var l3tmp_numPre : Int = 3 // [0-12] // has to be divisible by 2 for Jac if l3tmp_useSlotsForJac or l3tmp_useSlotVariables are disabled
  var l3tmp_numPost : Int = 3 // [0-12] // has to be divisible by 2 for Jac if l3tmp_useSlotsForJac or l3tmp_useSlotVariables are disabled
  var l3tmp_omega : Double = (if ("Jac" == l3tmp_smoother) 0.8 else 1.0) // [0.1-10.0] // the relaxation parameter to be used for the l3tmp_smoother
  var l3tmp_genStencilStencilConv : Boolean = false // [true|false] // tests stencil-stencil convolutions by using RAP instead of A
  var l3tmp_genStencilFields : Boolean = false // [true|false] // generates stencil fields that are used to store stencils of A (or RAP if l3tmp_genStencilStencilConv is true)
  var l3tmp_genAsyncCommunication : Boolean = false // [true|false] // replaces some sync communication statements in the L4 DSL file with their async counterparts 
  var l3tmp_genTemporalBlocking : Boolean = false // [true|false] // adds the necessary statements to the L4 DSL file to implement temporal blocking; adapts field layouts as well
  var l3tmp_tempBlockingMinLevel : Int = 1 // [0|maxLevel] // specifies a threshold for adding temporal blocking to generated l4 files; only levels larger or equal to this threshold are blocked
  var l3tmp_useConditionsForRBGS : Boolean = true // [true|false] // uses conditions to realize red-black patterns (as opposed to adapted offsets and strides)
  var l3tmp_useSlotsForJac : Boolean = true // [true|false] // uses sloted solution fields for Jacobi (as opposed to multiple distinct fields)
  var l3tmp_useSlotVariables : Boolean = true // [true|false] // uses slot variables (curSlot, nextSlot, prevSlot) for access to slotted solution fields; allows for odd number of smoothing steps

  /// functionality test
  var l3tmp_genFunctionBC : Boolean = true // uses some basic 2D diriclet boundary conditions with function value 
  var l3tmp_genExtFields : Boolean = false // adds one or more external fields to the L4 DSL file to test generation of subsequent functions
  var l3tmp_genGlobalOmega : Boolean = false // treats l3tmp_omega as a global (modifiable) parameter 
  var l3tmp_genSetableStencil : Boolean = false // generates stencil weights as global variables instead of constant values
  var l3tmp_genVectorFields : Boolean = false // attempts to solve Poisson's equation for (l3tmp_numVecDims)D vectors; all components are solved independently
  var l3tmp_numVecDims : Int = (if (l3tmp_genVectorFields) 2 else 1) // number of components the PDE is to be solved for
  var l3tmp_genFragLoops : Boolean = true // adds fragment loops to the L4 DSL file
  var l3tmp_genEmbeddedDomain : Boolean = false // adds a second domain to perform all computations on; the new domain is one fragment smaller on each boundary 

  /// optional features
  var l3tmp_printFieldAtEnd : Boolean = false // prints the solution field at the end of the application (or the mean solution in l3tmp_kelvin's case)
  var l3tmp_initSolWithRand : Boolean = true // initializes the solution on the finest level with random values

  /// Student project - Kelvin
  var l3tmp_kelvin : Boolean = false // currently only works for 2D
  var l3tmp_kelvin_numSamples : Int = 10 // only required for l3tmp_kelvin; number of samples to be evaluated
  var l3tmp_kelvin_numHaloFrags : Int = 2 // only required for l3tmp_kelvin; number of halo fragments used to implement the open boundary approximation  

  /// Student project - Oleg
  var l3tmp_genAdvancedTimers : Boolean = false // this is to enable the usage of some new, currently highly experimental, timer classes; requires that the timer classes are provided
  var l3tmp_genTimersPerFunction : Boolean = false // generates different timers for each function in the mg cycle
  var l3tmp_genTimersPerLevel : Boolean = false // generates different timers for each (mg) level
  var l3tmp_genTimersForComm : Boolean = false // generates additional timers for the communication
  var l3tmp_genCommTimersPerLevel : Boolean = false // generates different communication timers for each level 

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
    Constraints.condEnsureValue(l3tmp_numPre, l3tmp_numPre - (l3tmp_numPre % 2), "Jac" == l3tmp_smoother && !l3tmp_useSlotsForJac && !l3tmp_useSlotVariables,
      "Number of pre-smoothing steps has to be divisible by 2")
    Constraints.condEnsureValue(l3tmp_numPost, l3tmp_numPost - (l3tmp_numPost % 2), "Jac" == l3tmp_smoother && !l3tmp_useSlotsForJac && !l3tmp_useSlotVariables,
      "Number of post-smoothing steps has to be divisible by 2")

    Constraints.condEnsureValue(l3tmp_numPre, 2, 0 == l3tmp_numPre && 0 == l3tmp_numPost, "(l3tmp_numPre + l3tmp_numPost) must be larger than zero")

    if ("Jac" == l3tmp_smoother) Constraints.updateValue(l3tmp_omega, 0.8) else Constraints.updateValue(l3tmp_omega, 1.0)

    Constraints.condEnsureValue(dimensionality, 2, l3tmp_kelvin, "only 2D is supported for l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_genStencilStencilConv, true, l3tmp_kelvin, "required by l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_genStencilFields, true, l3tmp_kelvin, "required by l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_printFieldAtEnd, true, l3tmp_kelvin, "required by l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_genFunctionBC, false, l3tmp_kelvin, "not compatible with l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_genSetableStencil, false, l3tmp_kelvin, "not compatible with l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_genVectorFields, false, l3tmp_kelvin, "not compatible with l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_genEmbeddedDomain, false, l3tmp_kelvin, "not compatible with l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_initSolWithRand, false, l3tmp_kelvin, "not compatible with l3tmp_kelvin")

    Constraints.condEnsureValue(l3tmp_genAsyncCommunication, false, 26 != comm_strategyFragment, "invalid comm_strategyFragment")

    Constraints.condWarn("RBGS" == l3tmp_smoother && !l3tmp_useConditionsForRBGS,
      s"Currently NOT using l3tmp_useConditionsForRBGS leads to a color mismatch at primitive boundaries and thus to a reduced convergence rate")

    Constraints.condEnsureValue(l3tmp_useSlotVariables, false, !l3tmp_useSlotsForJac, "invalid if not using l3tmp_useSlotsForJac")
    Constraints.condEnsureValue(l3tmp_numPost, l3tmp_numPre, l3tmp_genTemporalBlocking, "l3tmp_numPre and l3tmp_numPost have to be equal")
    Constraints.condEnsureValue(l3tmp_genTemporalBlocking, false, l3tmp_numPre != l3tmp_numPost, "l3tmp_numPre and l3tmp_numPost have to be equal")
    Constraints.condEnsureValue(l3tmp_tempBlockingMinLevel, math.ceil(math.log(l3tmp_numPre) / math.log(2)).toInt,
      l3tmp_genTemporalBlocking && l3tmp_tempBlockingMinLevel < math.ceil(math.log(l3tmp_numPre) / math.log(2)).toInt,
      "temporal blocking requires a sufficient count of inner layers")
    Constraints.condEnsureValue(l3tmp_tempBlockingMinLevel, 1, l3tmp_genTemporalBlocking && l3tmp_tempBlockingMinLevel < 1, "l3tmp_tempBlockingMinLevel must be larger than zero (no blocking on the coarsest level possible)")
    Constraints.condEnsureValue(l3tmp_tempBlockingMinLevel, maxLevel, l3tmp_genTemporalBlocking && l3tmp_tempBlockingMinLevel > maxLevel, "l3tmp_tempBlockingMinLevel must be smaller or equal to maxLevel to enable temporal blocking")
    Constraints.condEnsureValue(l3tmp_tempBlockingMinLevel, 1, !l3tmp_genTemporalBlocking, "l3tmp_tempBlockingMinLevel reset to default for deactivated l3tmp_genTemporalBlocking")

    Constraints.condEnsureValue(l3tmp_genFunctionBC, false, 2 != dimensionality, "l3tmp_genFunctionBC is only valid for 2D problems")
    Constraints.condEnsureValue(l3tmp_initSolWithRand, true, !l3tmp_genFunctionBC && !l3tmp_kelvin, "initial solution of zero corresponds to the exact solution if l3tmp_genFunctionBC is false")
    Constraints.condEnsureValue(l3tmp_initSolWithRand, false, l3tmp_genFunctionBC, "l3tmp_genFunctionBC requires initial solution of zero")

    Constraints.condEnsureValue(l3tmp_numVecDims, 1, !l3tmp_genVectorFields, "vector dimensions larger than 1 are only allowed in conjunction with vector fields")
    Constraints.condEnsureValue(l3tmp_numVecDims, 2, l3tmp_genVectorFields && l3tmp_numVecDims <= 1, "vector dimensions must be larger than 1 when using vector fields")

    Constraints.condEnsureValue(l3tmp_genTimersPerFunction, false, !l3tmp_genAdvancedTimers, "requires l3tmp_genAdvancedTimers to be activated")
    Constraints.condEnsureValue(l3tmp_genTimersPerLevel, false, !l3tmp_genAdvancedTimers, "requires l3tmp_genAdvancedTimers to be activated")
    Constraints.condEnsureValue(l3tmp_genTimersForComm, false, !l3tmp_genAdvancedTimers, "requires l3tmp_genAdvancedTimers to be activated")
    Constraints.condEnsureValue(l3tmp_genCommTimersPerLevel, false, !l3tmp_genAdvancedTimers, "requires l3tmp_genAdvancedTimers to be activated")

    Constraints.condEnsureValue(l3tmp_genTimersForComm, false, l3tmp_genAsyncCommunication, "timers for overlapping communication are not yet supported")
    Constraints.condEnsureValue(l3tmp_genCommTimersPerLevel, false, !l3tmp_genTimersForComm, "requires l3tmp_genTimersForComm to be activated")

    Constraints.condEnsureValue(comm_useLevelIndependentFcts, false, l3tmp_genFunctionBC, "level independent communication functions are not compatible with non-trivial boundary conditions")
    Constraints.condEnsureValue(mpi_useCustomDatatypes, false, comm_useLevelIndependentFcts, "MPI data types cannot be used in combination with level independent communication functions yet")
  }
}
