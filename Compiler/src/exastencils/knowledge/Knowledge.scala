package exastencils.knowledge

import exastencils.constraints.Constraints
import exastencils.logger._
import exastencils.spl._

object Knowledge {
  // TODO: rename and move to hw knowledge?
  var targetCompiler : String = "MSVC" // the target compiler; may atm be "MSVC", "GCC", "IBMXL", "IBMBG"
  var targetCompilerVersion : Int = 0 // major version of the target compiler
  var targetCompilerVersionMinor : Int = 0 // minor version of the target compiler

  var useDblPrecision : Boolean = true

  var simd_instructionSet : String = "AVX" // currently allowed: "SSE3", "AVX", "AVX2", "QPX"
  def simd_vectorSize : Int = { // number of vector elements for SIMD instructions (currently only double precision)
    val double : Int = if (useDblPrecision) 1 else 2
    simd_instructionSet match {
      case "SSE3"         => 2 * double
      case "AVX" | "AVX2" => 4 * double
      case "QPX"          => 4 // yes, it's always 4
    }
  }
  def simd_header : String = { // header for vector intrinsics
    simd_instructionSet match {
      case "SSE3" | "AVX" | "AVX2" => "immintrin.h"
      case "QPX"                   => null
    }
  }

  var simd_avoidUnaligned : Boolean = false

  var useFasterExpand : Boolean = true

  var timer_type : String = "Chrono" // may be one of the following: 'Chrono', 'QPC', 'WIN_TIME', 'UNIX_TIME', 'MPI_TIME', 'RDSC', 'WINDOWS_RDSC'

  // === Layer 1 ===

  var dimensionality : Int = 3 // dimensionality of the problem; may be 1, 2 or 3

  // === Layer 2 ===

  // TODO: check if these parameters will be necessary or can be implicitly assumed once an appropriate field collection is in place
  var minLevel : Int = 0 // the coarsest level
  var maxLevel : Int = 6 // the finest level
  def numLevels : Int = (maxLevel - minLevel + 1) // the number of levels -> this assumes that the cycle descents to the coarsest level

  // --- Domain Decomposition ---

  /// general flags and information

  // specifies if domains are to be read from file
  var domain_readFromFile : Boolean = false

  // specifies if only rectangular domains are used 
  var domain_onlyRectangular : Boolean = true

  // the total number of blocks - in case of domain_generateRectengular this is the product of domain_generate_numBlocks_{x|y|z}
  var domain_numBlocks : Int = 1

  // the number of fragments per block - in case of domain_generateRectengular this is the product of domain_generate_numFragsPerBlock_{x|y|z}
  var domain_numFragmentsPerBlock : Int = 1

  // the total number of fragments the computational domain is partitioned into
  def domain_numFragmentsTotal : Int = domain_numBlocks * domain_numFragmentsPerBlock

  // the length of each fragment per dimension - this will either be one or specify the length in unit-fragments, i.e. the number of aggregated fragments per dimension
  var domain_fragmentLength_x : Int = 1
  var domain_fragmentLength_y : Int = 1
  var domain_fragmentLength_z : Int = 1
  def domain_fragmentLengthAsVec : Array[Int] = Array(domain_fragmentLength_x, domain_fragmentLength_y, domain_fragmentLength_z)

  /// specific flags for setting rectangular domains

  // specifies if dynamic domain setup code is to be generated for rectangular domains
  var domain_rect_generate : Boolean = true

  // number of blocks to be generated per dimension - one block will usually be mapped to one MPI thread
  var domain_rect_numBlocks_x : Int = 1
  var domain_rect_numBlocks_y : Int = 1
  var domain_rect_numBlocks_z : Int = 1

  // number of fragments to be generated for each block per dimension - this will usually be one or be equal to the number of OMP threads per dimension
  var domain_rect_numFragsPerBlock_x : Int = 1
  var domain_rect_numFragsPerBlock_y : Int = 1
  var domain_rect_numFragsPerBlock_z : Int = 1

  // the total number of fragments to be generated per dimension
  def domain_rect_numFragsTotal_x : Int = domain_rect_numFragsPerBlock_x * domain_rect_numBlocks_x
  def domain_rect_numFragsTotal_y : Int = domain_rect_numFragsPerBlock_y * domain_rect_numBlocks_y
  def domain_rect_numFragsTotal_z : Int = domain_rect_numFragsPerBlock_z * domain_rect_numBlocks_z
  def domain_rect_numFragsTotal : Int = domain_rect_numFragsTotal_x * domain_rect_numFragsTotal_y * domain_rect_numFragsTotal_z
  def domain_rect_numFragsTotalAsVec : Array[Int] = Array(domain_rect_numFragsTotal_x, domain_rect_numFragsTotal_y, domain_rect_numFragsTotal_z)

  // TODO:  var domain_gridWidth_x,y,z

  /// utility functions
  def domain_canHaveLocalNeighs : Boolean = (domain_numFragmentsPerBlock > 1) // specifies if fragments can have local (i.e.\ shared memory) neighbors, i.e.\ if local comm is required
  def domain_canHaveRemoteNeighs : Boolean = (domain_numBlocks > 1) // specifies if fragments can have remote (i.e.\ different mpi rank) neighbors, i.e.\ if mpi comm is required

  /// Student project - Jeremias
  var domain_useCase : String = "" //atm only "L-Shape" in 2D possible;  needs to be specified in case of onlyRectangular,rect_generate = false

  // TODO: ignore for IDE support for now
  var discr_hx : Array[Double] = Array() // grid widths in x direction per level
  var discr_hy : Array[Double] = Array() // grid widths in y direction per level
  var discr_hz : Array[Double] = Array() // grid widths in z direction per level

  // === Layer 3 ===

  // === Layer 4 ===

  // === Post Layer 4 ===
  var ir_genSepLayoutsPerField : Boolean = true // specifies if shared fieldlayouts should be duplicated when progressing from l4 to ir

  // --- Compiler Capabilities ---
  def supports_initializerList = { // indicates if the compiler supports initializer lists (e.g. for std::min)
    targetCompiler match {
      case "MSVC"            => targetCompilerVersion >= 18
      case "GCC"             => targetCompilerVersion > 4 || (targetCompilerVersion == 4 && targetCompilerVersionMinor >= 5)
      case "IBMXL" | "IBMBG" => false // TODO: does it support initializer lists? since which version?
      case _                 => Logger.error("Unsupported target compiler"); false
    }
  }

  // --- Data Structures ---
  var data_initAllFieldsWithZero : Boolean = true // specifies if all data points in all fields on all levels should initially be set zero (before the l4 initField functions are applied)
  var data_useFieldNamesAsIdx : Boolean = true // specifies if generated data field names should hold the clear text field identifier

  var data_alignFieldPointers : Boolean = false // specifies if pointers to field data are to be aligned to simd_vectorSize, e.g. to ensure correct alignment for SIMD accesses
  var data_alignTmpBufferPointers : Boolean = false // specifies if pointers to communication buffers are to be aligned to simd_vectorSize, e.g. to ensure correct alignment for SIMD accesses

  // --- OpenMP and MPI Parallelization ---
  var comm_strategyFragment : Int = 6 // [6|26] // specifies if communication is only performed along coordinate axis or to all neighbors
  var comm_useFragmentLoopsForEachOp : Boolean = true // [true|false] // specifies if comm ops (buffer copy, send/ recv, wait) should each be aggregated and handled in distinct fragment loops 

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
  var omp_enabled : Boolean = false // [true|false] 
  var omp_numThreads : Int = 1 // TODO // the number of omp threads to be used; may be incorporated in omp pragmas

  def omp_version : Double = { // the maximum version of omp supported by the chosen compiler
    targetCompiler match {
      case "MSVC"            => 2.0
      case "GCC"             => 4.0
      case "IBMXL" | "IBMBG" => 3.0
      case _                 => Logger.error("Unsupported target compiler"); 0.0
    }
  }
  var omp_parallelizeLoopOverFragments : Boolean = true // [true|false] // specifies if loops over fragments may be parallelized with omp if marked correspondingly
  var omp_parallelizeLoopOverDimensions : Boolean = false // [true|false] // specifies if loops over dimensions may be parallelized with omp if marked correspondingly
  var omp_useCollapse : Boolean = false // [true|false] // if true the 'collapse' directive may be used in omp for regions; this will only be done if the minimum omp version supports this
  var omp_minWorkItemsPerThread : Int = 400 // [1-inf] // threshold specifying which loops yield enough workload to amortize the omp overhead
  def omp_requiresCriticalSections : Boolean = { // true if the chosen compiler / mpi version requires critical sections to be marked explicitly
    targetCompiler match {
      case "MSVC"            => true
      case "GCC"             => true
      case "IBMXL" | "IBMBG" => false
      case _                 => Logger.error("Unsupported target compiler"); true
    }
  }

  // --- MPI Parallelization ---
  var mpi_enabled : Boolean = true // [true|false]
  var mpi_numThreads : Int = 1 // TODO // the number of mpi threads to be used 

  var mpi_useCustomDatatypes : Boolean = false // [true|false] // allows to use custom mpi data types when reading from/ writing to fields thus circumventing temp send/ receive buffers
  var mpi_useLoopsWherePossible : Boolean = true // [true|false] // allows to summarize some code blocks into loops in order to shorten the resulting code length
  var mpi_defaultCommunicator : String = "MPI_COMM_WORLD" // sets the initial communicator used by most MPI operations

  // --- Polyhedron Optimization ---
  // TODO: Alex: range of the following options
  var poly_optLevel_fine : Int = 0 // [0-3] // poly opt-level for {$poly_numFinestLevels} finest fields  0: disable (fastest);  3: aggressive (slowest)
  var poly_optLevel_coarse : Int = 0 // [0-poly_optLevel_fine] // polyhedral optimization level for coarsest fields  0: disable (fastest);  3: aggressive (slowest)
  var poly_numFinestLevels : Int = 2 // [1-numLevels] // number of levels that should be optimized in PolyOpt (starting from the finest)
  var poly_tileSize_x : Int = 1000000000 // [32-inf]
  var poly_tileSize_y : Int = 1000000000 // [16-inf]
  var poly_tileSize_z : Int = 1000000000 // [16-inf]
  var poly_tileSize_w : Int = 1000000000 // [16-inf]
  var poly_tileOuterLoop : Boolean = false // [true|false] // specify separately if the outermost loop should be tiled
  var poly_scheduleAlgorithm : String = "isl" // [isl|feautrier] // choose which schedule algorithm should be used in PolyOpt
  var poly_optimizeDeps : String = "rar" // [all|raw|rar] // specifies which dependences should be optimized; "all" means all validity dependences (raw, war, waw)
  var poly_simplifyDeps : Boolean = true // [true|false] // simplify dependences before computing a new schedule; this reduces PolyOpt run-time, but it could also lead to slower generated code
  var poly_fusionStrategy : String = "max" // [min|max] // specifies the level of fusion for the polyhedral scheduler
  var poly_maximizeBandDepth : Boolean = false // [true|false] // split bands as early as possible during schedule generation
  var poly_maxConstantTerm : Int = -1 // [(-1)-inf] // enforces that the constant coefficients in the calculated schedule are not larger than the maximal constant term (this can significantly increase the speed of the scheduling calculation; -1 means unlimited)
  var poly_maxCoefficient : Int = -1 // [(-1)-inf] // enforces that the coefficients for variable and parameter dimensions in the calculated schedule are not larger than the specified value (this can significantly increase the speed of the scheduling calculation; -1 means unlimited)

  // --- Other Optimizations ---
  var opt_useAddressPrecalc : Boolean = false // [true|false]
  var opt_vectorize : Boolean = false // [true|false]
  var opt_unroll : Int = 1 // [1-8]
  var opt_unroll_interleave : Boolean = false // [true|false] // FIXME: WARNING: there is a bug in combination with poly opts, use with caution until it's fixed!
  var opt_useColorSplitting : Boolean = false // [true|false] // only relevant for RBGS smoother currently

  /// BEGIN HACK config options for generating L4 DSL file
  var l3tmp_generateL4 : Boolean = true // generates a new Layer 4 file using the corresponding filename from Settings; the generated DSL file can is based on the following parameters

  /// SPL connected
  var l3tmp_smoother : String = "Jac" // [Jac|GS|RBGS] // the l3tmp_smoother to be generated
  var l3tmp_cgs : String = "CG" // [CG] // the coarse grid solver to be generated
  var l3tmp_numRecCycleCalls : Int = 1 // [1-2] // 1 corresponds to v-cycles while 2 corresponds to w-cycles
  var l3tmp_numPre : Int = 3 // [0-12] // has to be divisible by 2 for Jac if l3tmp_useSlotsForJac or l3tmp_useSlotVariables are disabled
  var l3tmp_numPost : Int = 3 // [0-12] // has to be divisible by 2 for Jac if l3tmp_useSlotsForJac or l3tmp_useSlotVariables are disabled
  var l3tmp_omega : Double = (if ("Jac" == l3tmp_smoother) 0.8 else 1.0) // [0.1-10.0] // the relaxation parameter to be used for the l3tmp_smoother
  var l3tmp_genStencilStencilConv : Boolean = false // [true|false] // tests stencil-stencil convolutions by using RAP instead of A
  var l3tmp_genStencilFields : Boolean = false // [true|false] // generates stencil fields that are used to store stencils of A (or RAP if l3tmp_genStencilStencilConv is true)
  var l3tmp_genAsyncCommunication : Boolean = false // [true|false] // replaces some sync communication statements in the L4 DSL file with their async counterparts 
  var l3tmp_genTemporalBlocking : Boolean = false // [true|false] // adds the necessary statements to the L4 DSL file to implement temporal blocking; adapts field layouts as well
  var l3tmp_tempBlockingMinLevel : Int = 1 // [1+minLevel|maxLevel] // specifies a threshold for adding temporal blocking to generated l4 files; only levels larger or equal to this threshold are blocked
  var l3tmp_useConditionsForRBGS : Boolean = true // [true|false] // uses conditions to realize red-black patterns (as opposed to adapted offsets and strides)
  var l3tmp_useSlotsForJac : Boolean = true // [true|false] // uses sloted solution fields for Jacobi (as opposed to multiple distinct fields)
  var l3tmp_useSlotVariables : Boolean = true // [true|false] // uses slot variables (currentSlot, nextSlot, previousSlot) for access to slotted solution fields; allows for odd number of smoothing steps
  var l3tmp_genHDepStencils : Boolean = false // [true|false] // generates stencils dependent on the grid width h

  /// timer generation
  var l3tmp_genTimersPerFunction : Boolean = false // generates different timers for each function in the mg cycle
  var l3tmp_genTimersPerLevel : Boolean = false // generates different timers for each (mg) level
  var l3tmp_genTimersForComm : Boolean = false // generates additional timers for the communication
  var l3tmp_genCommTimersPerLevel : Boolean = false // generates different communication timers for each level

  var l3tmp_printAllTimers : Boolean = false // prints results for all used timers at the end of the application
  var l3tmp_printTimersToFile : Boolean = false // prints results for all used timers at the end of the application; uses l3tmp_timerOuputFile as target file
  var l3tmp_timerOuputFile : String = "timings.csv" // the file timer data is to be written to if l3tmp_printTimersToFile is activated

  /// functionality test
  var l3tmp_exactSolution : String = "Zero" // specifies which function (type) is used for the solution/ rhs is used; allowed options are 'Zero', 'Polynomial', 'Trigonometric' and 'Kappa'
  var l3tmp_genNonZeroRhs : Boolean = false // generates more complex variants of the chosen solution function resulting in non-trival right hand sides
  var l3tmp_genExtFields : Boolean = false // adds one or more external fields to the L4 DSL file to test generation of subsequent functions
  var l3tmp_genGlobalOmega : Boolean = false // treats l3tmp_omega as a global (modifiable) parameter 
  var l3tmp_genSetableStencil : Boolean = false // generates stencil weights as global variables instead of constant values
  var l3tmp_genVectorFields : Boolean = false // attempts to solve Poisson's equation for (l3tmp_numVecDims)D vectors; all components are solved independently
  var l3tmp_numVecDims : Int = (if (l3tmp_genVectorFields) 2 else 1) // number of components the PDE is to be solved for
  var l3tmp_genFragLoops : Boolean = true // adds fragment loops to the L4 DSL file
  var l3tmp_genEmbeddedDomain : Boolean = false // adds a second domain to perform all computations on; the new domain is one fragment smaller on each boundary
  var l3tmp_useMaxNorm : Boolean = false // uses the maximum norm instead of the L2 norm when reducing the residual on the finest level
  var l3tmp_genCellBasedDiscr : Boolean = false // sets up a cell based discretization

  /// optional features
  var l3tmp_printFieldAtEnd : Boolean = false // prints the solution field at the end of the application (or the mean solution in l3tmp_kelvin's case)
  var l3tmp_initSolWithRand : Boolean = true // initializes the solution on the finest level with random values
  var l3tmp_genForAutoTests : Boolean = false // generates code for automatic testing purposes - if l3tmp_printError is activated NO residual is printed
  var l3tmp_printError : Boolean = false // generates code that calculates and prints the error in each iteration
  var l3tmp_useMaxNormForError : Boolean = true // uses the maximum norm instead of the L2 norm when reducing the error

  /// Paper project - SISC
  var l3tmp_sisc : Boolean = false // generates test problems for the upcomming SISC paper in conjunction with dimensionality and l3tmp_genStencilFields

  /// Student project - Kelvin
  var l3tmp_kelvin : Boolean = false // currently only works for 2D
  var l3tmp_kelvin_numSamples : Int = 10 // only required for l3tmp_kelvin; number of samples to be evaluated
  var l3tmp_kelvin_numHaloFrags : Int = 2 // only required for l3tmp_kelvin; number of halo fragments used to implement the open boundary approximation  

  /// experimental features
  var experimental_useLevelIndepFcts : Boolean = false

  var experimental_Neumann : Boolean = false // highly experimental -> use only if you know what you are doing
  var experimental_NeumannOrder : Int = 2 // may currently be 1 or 2
  var experimental_NeumannNormalize : Boolean = false // normalize solution after each v-cycle

  var experimental_timerEnableCallStacks : Boolean = false // generates call stacks for all employed timers

  /// END HACK

  def update(configuration : Configuration = new Configuration) : Unit = {
    // NOTE: it is required to call update at least once

    // specific project configurations - SISC
    Constraints.condEnsureValue(l3tmp_exactSolution, "Kappa", l3tmp_sisc && l3tmp_genStencilFields, "Kappa is required as l3tmp_exactSolution for variable stencils and l3tmp_sisc")
    Constraints.condEnsureValue(l3tmp_genHDepStencils, true, l3tmp_sisc && l3tmp_genStencilFields, "l3tmp_genHDepStencils must be true for variable stencils and l3tmp_sisc")
    Constraints.condEnsureValue(experimental_Neumann, false, l3tmp_sisc, "Neumann boundary conditions are not compatible with l3tmp_sisc")

    Constraints.condEnsureValue(l3tmp_genNonZeroRhs, true, "Kappa" == l3tmp_exactSolution, "Kappa requires l3tmp_genNonZeroRhs")

    // specific project configurations - Kelvin
    Constraints.condEnsureValue(dimensionality, 2, l3tmp_kelvin, "only 2D is supported for l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_genStencilStencilConv, true, l3tmp_kelvin, "required by l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_genStencilFields, true, l3tmp_kelvin, "required by l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_printFieldAtEnd, true, l3tmp_kelvin, "required by l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_exactSolution, "Zero", l3tmp_kelvin, "l3tmp_kelvin requires the options of a zero solution")
    Constraints.condEnsureValue(l3tmp_genSetableStencil, false, l3tmp_kelvin, "not compatible with l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_genVectorFields, false, l3tmp_kelvin, "not compatible with l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_genEmbeddedDomain, false, l3tmp_kelvin, "not compatible with l3tmp_kelvin")
    Constraints.condEnsureValue(l3tmp_initSolWithRand, false, l3tmp_kelvin, "not compatible with l3tmp_kelvin")

    // domain
    Constraints.condEnsureValue(domain_rect_generate, false, !domain_onlyRectangular, "only rectangular domains can be generated")
    Constraints.condEnsureValue(domain_readFromFile, true, !domain_rect_generate && domain_useCase == "", "non-generated domains must be read from file")
    Constraints.condEnsureValue(domain_rect_generate, false, domain_readFromFile, "domain_rect_generate is not allowed if domain_readFromFile is enabled")

    Constraints.condEnsureValue(domain_rect_numBlocks_y, 1, domain_rect_generate && dimensionality < 2, "domain_rect_numBlocks_y must be set to 1 for problems with a dimensionality smaller 2")
    Constraints.condEnsureValue(domain_rect_numBlocks_z, 1, domain_rect_generate && dimensionality < 3, "domain_rect_numBlocks_z must be set to 1 for problems with a dimensionality smaller 3")
    Constraints.condEnsureValue(domain_rect_numFragsPerBlock_y, 1, domain_rect_generate && dimensionality < 2, "domain_rect_numFragsPerBlock_y must be set to 1 for problems with a dimensionality smaller 2")
    Constraints.condEnsureValue(domain_rect_numFragsPerBlock_z, 1, domain_rect_generate && dimensionality < 3, "domain_rect_numFragsPerBlock_z must be set to 1 for problems with a dimensionality smaller 3")

    Constraints.condEnsureValue(domain_fragmentLength_y, 1, dimensionality < 2, "domain_fragmentLength_y must be set to 1 for problems with a dimensionality smaller 2")
    Constraints.condEnsureValue(domain_fragmentLength_z, 1, dimensionality < 3, "domain_fragmentLength_z must be set to 1 for problems with a dimensionality smaller 3")

    if (domain_rect_generate) {
      Constraints.updateValue(domain_numBlocks, domain_rect_numBlocks_x * domain_rect_numBlocks_y * domain_rect_numBlocks_z)
      Constraints.updateValue(domain_numFragmentsPerBlock, domain_rect_numFragsPerBlock_x * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_z)
    }

    Constraints.condEnsureValue(minLevel, 0, minLevel < 0, "minLevel must not be negative")
    Constraints.condEnsureValue(maxLevel, 1, maxLevel <= 0, "maxLevel must not be smaller or equal to 0")
    Constraints.condEnsureValue(minLevel, maxLevel - 1, minLevel >= maxLevel, "maxLevel must be larger than minLevel")

    // l3tmp - problem to solve
    Constraints.condEnsureValue(l3tmp_genNonZeroRhs, true, experimental_Neumann, "l3tmp_genNonZeroRhs is required for Neumann boundary conditions")
    Constraints.condEnsureValue(l3tmp_exactSolution, "Trigonometric", experimental_Neumann, "l3tmp_genNonZeroRhs is required for Neumann boundary conditions")
    Constraints.condEnsureValue(l3tmp_genNonZeroRhs, false, "Polynomial" != l3tmp_exactSolution && "Kappa" != l3tmp_exactSolution && !experimental_Neumann, "non-trivial rhs are currently only supported for Polynomial and Kappa cases")

    Constraints.condEnsureValue(experimental_NeumannOrder, 1, experimental_Neumann && l3tmp_genCellBasedDiscr, "experimental_OrderNeumann must be 1 for cell based discretizations")
    Constraints.condEnsureValue(experimental_NeumannOrder, 2, experimental_Neumann && experimental_NeumannOrder < 1 || experimental_NeumannOrder > 2, "experimental_OrderNeumann must be between 1 and 2")

    Constraints.condEnsureValue(l3tmp_initSolWithRand, true, "Zero" == l3tmp_exactSolution && !l3tmp_kelvin, "initial solution of zero corresponds to the exact solution if l3tmp_genFunctionBC is false")
    Constraints.condEnsureValue(l3tmp_initSolWithRand, false, "Zero" != l3tmp_exactSolution, "l3tmp_exactSolution not equal to zero requires initial solution of zero")

    Constraints.condEnsureValue(l3tmp_numVecDims, 1, !l3tmp_genVectorFields, "vector dimensions larger than 1 are only allowed in conjunction with vector fields")
    Constraints.condEnsureValue(l3tmp_numVecDims, 2, l3tmp_genVectorFields && l3tmp_numVecDims <= 1, "vector dimensions must be larger than 1 when using vector fields")

    // l3tmp - stencils
    Constraints.condEnsureValue(l3tmp_genStencilFields, false, experimental_Neumann, "l3tmp_genStencilFields is currently not compatible with Neumann boundary conditions")
    Constraints.condEnsureValue(l3tmp_genStencilStencilConv, false, experimental_Neumann, "l3tmp_genStencilStencilConv is currently not compatible with Neumann boundary conditions")
    Constraints.condEnsureValue(l3tmp_genStencilFields, false, l3tmp_genCellBasedDiscr, "l3tmp_genStencilFields is currently not compatible with cell based discretizations")
    Constraints.condEnsureValue(l3tmp_genStencilStencilConv, false, l3tmp_genCellBasedDiscr, "l3tmp_genStencilStencilConv is currently not compatible with cell based discretizations")
    Constraints.condEnsureValue(l3tmp_genHDepStencils, true, experimental_Neumann, "l3tmp_genHDepStencils is required for Neumann boundary conditions")
    Constraints.condEnsureValue(l3tmp_genHDepStencils, true, l3tmp_genNonZeroRhs, "non-trivial rhs requires the usage of grid width dependent stencils")

    // l3tmp - multigrid config
    if ("Jac" == l3tmp_smoother) Constraints.updateValue(l3tmp_omega, 0.8) else Constraints.updateValue(l3tmp_omega, 1.0) // FIXME: required?

    Constraints.condEnsureValue(l3tmp_numPre, l3tmp_numPre - (l3tmp_numPre % 2), "Jac" == l3tmp_smoother && !l3tmp_useSlotsForJac,
      "Number of pre-smoothing steps has to be divisible by 2 if Jacobi is used but slotting is disabled")
    Constraints.condEnsureValue(l3tmp_numPost, l3tmp_numPost - (l3tmp_numPost % 2), "Jac" == l3tmp_smoother && !l3tmp_useSlotsForJac,
      "Number of post-smoothing steps has to be divisible by 2 if Jacobi is used but slotting is disabled")
    Constraints.condEnsureValue(l3tmp_numPre, 2, 0 == l3tmp_numPre && 0 == l3tmp_numPost, "(l3tmp_numPre + l3tmp_numPost) must be larger than zero")

    Constraints.condWarn("RBGS" == l3tmp_smoother && !l3tmp_useConditionsForRBGS, s"Currently NOT using l3tmp_useConditionsForRBGS leads to a color mismatch at primitive boundaries and thus to a reduced convergence rate")

    Constraints.condEnsureValue(l3tmp_useSlotVariables, false, !l3tmp_useSlotsForJac, "invalid if not using l3tmp_useSlotsForJac")

    // l3tmp - temporal blocking
    Constraints.condEnsureValue(l3tmp_genTemporalBlocking, false, experimental_Neumann, "l3tmp_genTemporalBlocking is currently not compatible with Neumann boundary conditions")
    Constraints.condEnsureValue(l3tmp_genTemporalBlocking, false, l3tmp_genCellBasedDiscr, "l3tmp_genTemporalBlocking is currently not compatible with cell based discretizations")
    Constraints.condEnsureValue(l3tmp_genTemporalBlocking, false, "RBGS" == l3tmp_smoother, "l3tmp_genTemporalBlocking is currently not compatible with RBGS smoothers")
    Constraints.condEnsureValue(l3tmp_genTemporalBlocking, false, l3tmp_numPre != l3tmp_numPost, "l3tmp_numPre and l3tmp_numPost have to be equal")
    Constraints.condEnsureValue(l3tmp_tempBlockingMinLevel, math.ceil(math.log(l3tmp_numPre) / math.log(2)).toInt,
      l3tmp_genTemporalBlocking && l3tmp_tempBlockingMinLevel < math.ceil(math.log(l3tmp_numPre) / math.log(2)).toInt,
      "temporal blocking requires a sufficient count of inner layers")
    Constraints.condEnsureValue(l3tmp_tempBlockingMinLevel, 1 + minLevel, l3tmp_genTemporalBlocking && l3tmp_tempBlockingMinLevel <= minLevel, "l3tmp_tempBlockingMinLevel must be larger than minLevel (no blocking on the coarsest level possible)")
    Constraints.condEnsureValue(l3tmp_tempBlockingMinLevel, maxLevel, l3tmp_genTemporalBlocking && l3tmp_tempBlockingMinLevel > maxLevel, "l3tmp_tempBlockingMinLevel must be smaller or equal to maxLevel to enable temporal blocking")
    Constraints.condEnsureValue(l3tmp_tempBlockingMinLevel, 1 + minLevel, !l3tmp_genTemporalBlocking, "l3tmp_tempBlockingMinLevel reset to default for deactivated l3tmp_genTemporalBlocking")

    Constraints.condEnsureValue(l3tmp_numPost, l3tmp_numPre, l3tmp_genTemporalBlocking, "l3tmp_numPre and l3tmp_numPost have to be equal")

    // l3tmp - parallelization
    Constraints.condEnsureValue(l3tmp_genAsyncCommunication, false, 26 != comm_strategyFragment, "invalid comm_strategyFragment")
    Constraints.condEnsureValue(l3tmp_genFragLoops, false, "RBGS" == l3tmp_smoother, "Currently fragment loops are not compatible with RBGS smoothers")

    // l3tmp - timer generation
    Constraints.condEnsureValue(l3tmp_genTimersForComm, false, l3tmp_genAsyncCommunication, "timers for overlapping communication are not yet supported")
    Constraints.condEnsureValue(l3tmp_genCommTimersPerLevel, false, !l3tmp_genTimersForComm, "requires l3tmp_genTimersForComm to be activated")

    // parallelization
    Constraints.condEnsureValue(omp_useCollapse, false, "IBMXL" == targetCompiler, "omp collapse is currently not fully supported by the IBM XL compiler")
    Constraints.condEnsureValue(omp_parallelizeLoopOverDimensions, false, omp_enabled && omp_parallelizeLoopOverFragments, "omp_parallelizeLoopOverDimensions and omp_parallelizeLoopOverFragments are mutually exclusive")

    Constraints.condWarn(mpi_numThreads != domain_numBlocks, s"The number of mpi threads ($mpi_numThreads) differs from the number of blocks ($domain_numBlocks) -> this might lead to unexpected behavior!")
    Constraints.condWarn(omp_parallelizeLoopOverFragments && omp_numThreads > domain_numFragmentsPerBlock, s"The number of omp threads ($omp_numThreads) is higher than the number of fragments per block ($domain_numFragmentsPerBlock) -> this will result in idle omp threads!")
    Constraints.condWarn(omp_parallelizeLoopOverFragments && 0 != domain_numFragmentsPerBlock % omp_numThreads, s"The number of fragments per block ($domain_numFragmentsPerBlock) is not divisible by the number of omp threads ($omp_numThreads) -> this might result in a severe load imbalance!")

    Constraints.condEnsureValue(experimental_useLevelIndepFcts, false, "Zero" != l3tmp_exactSolution, "level independent communication functions are not compatible with non-trivial boundary conditions")
    Constraints.condEnsureValue(mpi_useCustomDatatypes, false, experimental_useLevelIndepFcts, "MPI data types cannot be used in combination with level independent communication functions yet")

    // data    
    Constraints.condEnsureValue(data_alignFieldPointers, true, opt_vectorize && "QPX" == simd_instructionSet, "data_alignFieldPointers must be true for vectorization with QPX")

    Constraints.condEnsureValue(simd_avoidUnaligned, true, opt_vectorize && "QPX" == simd_instructionSet, "QPX does not support unaligned loads/stores")
    Constraints.condEnsureValue(simd_avoidUnaligned, false, !opt_vectorize, "avoid unaligned loads/stores doesn't make sense without vectorization enabled")
    Constraints.condEnsureValue(simd_avoidUnaligned, false, !data_alignFieldPointers, "impossible to avoid unaligned accesses if data is not aligned")

    // optimization
    Constraints.condEnsureValue(poly_optLevel_coarse, poly_optLevel_fine, poly_optLevel_coarse > poly_optLevel_fine, "optimization level for coarse grids must smaller or equal to the one for the fine levels")
    Constraints.condEnsureValue(poly_numFinestLevels, numLevels, poly_numFinestLevels > numLevels, "number of fine levels (for optimization) cannot exceed the number of all levels")
    Constraints.condEnsureValue(poly_maximizeBandDepth, true, poly_fusionStrategy == "min", "poly_maximizeBandDepth has no effect if poly_fusionStrategy is \"min\"")

    Constraints.condEnsureValue(opt_useColorSplitting, false, l3tmp_smoother != "RBGS", "color splitting is only relevant for RBGS smoother")

    Constraints.condEnsureValue(ir_genSepLayoutsPerField, true, opt_useColorSplitting, "color splitting requires separate field layouts")

    // timer configuration    
    Constraints.condEnsureValue(timer_type, "Chrono", !mpi_enabled && "MPI_TIME" == timer_type, "MPI_TIME is not supported for codes generated without MPI")
    Constraints.condEnsureValue(timer_type, "Chrono", "QPC" == timer_type && "MSVC" != targetCompiler, "QPC is only supported for windows")
    Constraints.condEnsureValue(timer_type, "WINDOWS_RDSC", "RDSC" == timer_type && "MSVC" == targetCompiler, "WINDOWS_RDSC is required for windows systems")
    Constraints.condEnsureValue(timer_type, "RDSC", "WINDOWS_RDSC" == timer_type && "MSVC" != targetCompiler, "RDSC is required for non-windows systems")
    Constraints.condEnsureValue(timer_type, "UNIX_TIME", "WIN_TIME" == timer_type && "MSVC" != targetCompiler, "WIN_TIME is not supported for non-windows systems")
    Constraints.condEnsureValue(timer_type, "WIN_TIME", "UNIX_TIME" == timer_type && "MSVC" == targetCompiler, "UNIX_TIME is not supported for windows systems")
    Constraints.condEnsureValue(timer_type, "UNIX_TIME", "Chrono" == timer_type && "IBMXL" == targetCompiler, "IBM XL does currently not support std::chrono")
    Constraints.condEnsureValue(timer_type, "UNIX_TIME", "Chrono" == timer_type && "IBMBG" == targetCompiler, "IBM BG does currently not support std::chrono")
  }
}
