package exastencils.config

import scala.collection.mutable.ListBuffer

import exastencils.constraints._

object Knowledge {
  // === Parser ===

  // adds annotations containing location information to all parsed nodes
  var parser_annotateLocation : Boolean = true

  /// === Layer 1 ===

  // dimensionality of the problem; may currently be 1, 2 or 3
  var dimensionality : Int = 3
  // shortcut to an array with dimension indices
  def dimensions = (0 until dimensionality).toArray

  /// === Layer 2 ===

  // [0~4§minLevel+1]  // nonSISC [0~12] // @constant // the coarsest level
  var minLevel : Int = 0
  // [4~12§maxLevel+1] // @constant // the finest level
  var maxLevel : Int = 6
  // the number of levels -> this assumes that the cycle descents to the coarsest level
  def numLevels : Int = maxLevel - minLevel + 1
  // shortcut to an array with level numbers
  def levels = (minLevel to maxLevel).toArray

  /// --- Domain Decomposition ---

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
  // TODO: allow more than three dimensions
  // [1~64§domain_fragmentLength_x*2]
  var domain_fragmentLength_x : Int = 1
  // [1~64§domain_fragmentLength_y*2]
  var domain_fragmentLength_y : Int = 1
  // [1~64§domain_fragmentLength_z*2]
  var domain_fragmentLength_z : Int = 1

  // shortcut to an array containing the values of domain_fragmentLength_*
  def domain_fragmentLengthAsVec : Array[Int] = Array(domain_fragmentLength_x, domain_fragmentLength_y, domain_fragmentLength_z)

  /// utility functions

  // evaluates if fragments can have local (i.e. same block) neighbors, i.e. if local comm is required
  def domain_canHaveLocalNeighs : Boolean = domain_numFragmentsPerBlock > 1 || domain_rect_hasPeriodicity
  // evaluates if fragments can have remote (i.e. different block) neighbors, i.e. if mpi comm is required
  def domain_canHaveRemoteNeighs : Boolean = domain_numBlocks > 1 || (mpi_enabled && domain_rect_hasPeriodicity)

  /// specific flags for setting rectangular domains

  // specifies if dynamic domain setup code is to be generated for rectangular domains
  var domain_rect_generate : Boolean = true

  // number of blocks to be generated per dimension - one block will usually be mapped to one MPI thread
  var domain_rect_numBlocks_x : Int = 1
  var domain_rect_numBlocks_y : Int = 1
  var domain_rect_numBlocks_z : Int = 1

  // shortcut to an array containing the values of domain_rect_numBlocks_*
  def domain_rect_numBlocksAsVec : Array[Int] = Array(domain_rect_numBlocks_x, domain_rect_numBlocks_y, domain_rect_numBlocks_z)

  // number of fragments to be generated for each block per dimension - this will usually be one or be equal to the number of OMP threads per dimension
  // [1~64§domain_rect_numFragsPerBlock_x*2]
  var domain_rect_numFragsPerBlock_x : Int = 1
  // [1~64§domain_rect_numFragsPerBlock_y*2]
  var domain_rect_numFragsPerBlock_y : Int = 1
  // [1~64§domain_rect_numFragsPerBlock_z*2]
  var domain_rect_numFragsPerBlock_z : Int = 1

  // shortcut to an array containing the values of domain_rect_numFragsPerBlock_*
  def domain_rect_numFragsPerBlockAsVec : Array[Int] = Array(domain_rect_numFragsPerBlock_x, domain_rect_numFragsPerBlock_y, domain_rect_numFragsPerBlock_z)

  // periodicity for rectangular domains
  var domain_rect_periodic_x : Boolean = false
  var domain_rect_periodic_y : Boolean = false
  var domain_rect_periodic_z : Boolean = false

  // shortcut to an array containing the values of domain_rect_periodic_*
  def domain_rect_periodicAsVec : Array[Boolean] = Array(domain_rect_periodic_x, domain_rect_periodic_y, domain_rect_periodic_z)
  // evaluates if any of domain_rect_periodic_* are true
  def domain_rect_hasPeriodicity : Boolean = domain_rect_periodic_x || domain_rect_periodic_y || domain_rect_periodic_z

  // the total number of fragments to be generated per dimension
  def domain_rect_numFragsTotal_x : Int = domain_rect_numFragsPerBlock_x * domain_rect_numBlocks_x
  def domain_rect_numFragsTotal_y : Int = domain_rect_numFragsPerBlock_y * domain_rect_numBlocks_y
  def domain_rect_numFragsTotal_z : Int = domain_rect_numFragsPerBlock_z * domain_rect_numBlocks_z

  // the product of the total number of fragments per dimension, i.e. the total number of fragments
  def domain_rect_numFragsTotal : Int = domain_rect_numFragsTotal_x * domain_rect_numFragsTotal_y * domain_rect_numFragsTotal_z
  // shortcut to an array containing the values of domain_rect_numFragsTotal_*
  def domain_rect_numFragsTotalAsVec : Array[Int] = Array(domain_rect_numFragsTotal_x, domain_rect_numFragsTotal_y, domain_rect_numFragsTotal_z)

  //// specifications for field

  // specifies precision of printField. If set to -1, default precision is used
  var field_printFieldPrecision : Int = -1

  /// specification of the computational (discretization) grid

  // specifies if the grid is uniform, i.e. if the grid width is constant per dimension and level
  var grid_isUniform : Boolean = true
  // specifies if a staggered grid is to be used
  var grid_isStaggered : Boolean = false
  // specifies if the grid is axis aligned
  var grid_isAxisAligned : Boolean = true

  // in case of non-uniform grids a grid spacing model must be specified
  // must be uniform if grid_isUniform; may be "diego", "diego2", "linearFct", or "blockstructured" otherwise
  var grid_spacingModel : String = "uniform"

  // specifies if staggered control volumes are to be halved at applicable boundaries (ie in the stagger dimension)
  var grid_halveStagBoundaryVolumes : Boolean = false

  /// discretization information

  // type of discretization
  // may be one of the following: 'FD' or 'finiteDifferences', 'FV' or 'finiteVolumes'
  var discr_type : String = "FD"

  // may currently be 1 or 2
  var discr_defaultNeumannOrder : Int = 1
  // allows setting the interpolation order for Dirichlet BCs for cell or face localized quantities
  var discr_defaultDirichletOrder : Int = 1

  // may currently be "Taylor" or "Lagrange"
  var discr_fd_scheme : String = "Taylor"

  // target error order for FD discretizations
  var discr_fd_order : Int = 2

  // (uniform) grid width per dimension and level
  var discr_hx : Array[Double] = Array()
  var discr_hy : Array[Double] = Array()
  var discr_hz : Array[Double] = Array()

  /// swe special flags
  var swe_stationsMax = 10

  /// === Layer 3 ===

  /// solver setup information

  // generate debug output (run-time) for generated L3 solvers if true
  var solver_generateDbgOutputs : Boolean = false

  // don't print any info messages in the generated solver (does not affect warnings)
  var solver_silent : Boolean = false

  // target reduction of the residual norm for the whole solver
  var solver_targetResReduction : Double = 1e-5

  // residual threshold for aborting the whole solver
  var solver_absResThreshold : Double = 0.0

  // maximum number of iterations for the whole solver
  var solver_maxNumIts : Int = 128

  // toggle usage of the full approximation scheme (FAS)
  var solver_useFAS : Boolean = false

  // coloring mode used in the generated smoother
  // may be one of the following: 'None', 'RB' or '2-way', '4-way' (2D only), '8-way' (3D only), '9-way' (2D only), '27-way' (3D only)
  var solver_smoother_coloring : String = "None"

  // damping factor used in the generated smoother
  var solver_smoother_damping : Double = 1.0

  // number of pre-smoothing steps
  var solver_smoother_numPre : Int = 3

  // number of post-smoothing steps
  var solver_smoother_numPost : Int = 3

  // determines if the smoother implements Jacobi-type updates
  var solver_smoother_jacobiType : Boolean = false

  // coarse grid solver
  // may be one of the following: 'CG' or 'ConjugateGradient', 'BiCGStab', 'MinRes', 'ConjugateResidual', 'Smoother'
  var solver_cgs : String = "CG"

  // maximum number of coarse grid solver iterations
  var solver_cgs_maxNumIts : Int = 512

  // target reduction of the residual norm for the coarse grid solver
  var solver_cgs_targetResReduction : Double = 0.001

  // residual threshold for aborting the cgs
  var solver_cgs_absResThreshold : Double = 0.0

  // restart cgs after solver_cgs_restartAfter iterations if true
  var solver_cgs_restart : Boolean = false

  // restart interval for the cgs
  var solver_cgs_restartAfter : Int = 1000

  // if true, loops containing a single local solve node will be split into halo and inner; inner will be generated without conditionals
  var solver_splitLocalSolveLoops : Boolean = false

  // if true, all equations to be solved that are not on the finest level are shuffled and get their rhs eliminated
  var solver_eliminateRHS : Boolean = true

  // if true, error fields are generated to replace solutions on coarser grids; adapts boundary conditions
  var solver_overwriteSolutionFields : Boolean = true

  /// === Layer 4 ===

  // specifies if shared fieldlayouts should be duplicated when progressing from l4 to ir
  var l4_genSepLayoutsPerField : Boolean = true

  // generates a default application assuming a generated solver _if_ no application function is provided
  var l4_genDefaultApplication : Boolean = true

  var l4_defAppl_FieldToPrint : String = ""

  // generates a default SoA to AoS transformation _if_ no LayoutTransformation block exists
  //   (such a transformation is only created for fields whose level specification exactly matches the one of the associated layout,
  //     or the layout is flagged with '@all')
  var l4_genSoA2AoSTransformation : Boolean = false

  /// === Post Layer 4 ===

  /// --- general parameters ---

  // use double precision for floating point numbers if true and single precision otherwise
  var useDblPrecision : Boolean = true

  // use strategy which allows expanding all applicable nodes (recursively) in only one step
  // TODO: are there any use cases where this is not reasonable?
  var useFasterExpand : Boolean = true

  /// --- testing ---

  // flag that can be used to trigger testing specific code (in ExaSlang or in the generator)
  var testing_enabled : Boolean = false

  // flag that can be used in ExaSlang to trigger printing the residual (has to be implemented manually)
  var testing_printRes : Boolean = true

  // flag that can be used in ExaSlang to trigger printing the error (has to be implemented manually)
  var testing_printErr : Boolean = true

  // maximum precision for printing values in testing environments (for printWithReducedPrec calls)
  var testing_maxPrecision : Int = 4

  // threshold for assuming values as zero when printing in testing environments (for printWithReducedPrec calls)
  var testing_zeroThreshold : Double = 1e-12

  /// --- timing / benchmarking ---

  // type of the timers to be generated
  // may be one of the following: 'Chrono', 'QPC', 'WIN_TIME', 'UNIX_TIME', 'MPI_TIME', 'RDSC', 'WINDOWS_RDSC'
  var timer_type : String = "Chrono"

  var timer_printTimersToFileForEachRank : Boolean = false
  // prints separate timer values for each rank -> requires some additional memory for the gather op

  // library/tool to use for benchmarking
  // may be one of the following: 'None', 'likwid'
  var benchmark_backend = "None"

  /// --- interfacing ---

  // generates fortran compliant function names and marks functions for interfacing
  var generateFortranInterface : Boolean = false

  // generates plain C interfaces
  var generateCInterface : Boolean = false

  // adds the CImg library to the generated solver
  var library_CImg : Boolean = false

  // --- data structure ---

  // specifies if all data points in all fields on all levels should initially be set zero (before the l4 initField functions are applied)
  var data_initAllFieldsWithZero : Boolean = true
  // specifies if generated data field names should hold the clear text field identifier
  var data_useFieldNamesAsIdx : Boolean = true

  // specifies if pointers to field data are to be aligned to simd_vectorSize, e.g. to ensure correct alignment for SIMD accesses
  var data_alignFieldPointers : Boolean = false
  // specifies if pointers to communication buffers are to be aligned to simd_vectorSize, e.g. to ensure correct alignment for SIMD accesses
  var data_alignTmpBufferPointers : Boolean = false

  // generates global variables for employed field indices such as ghost/duplicate/inner begin and end
  var data_genVariableFieldSizes : Boolean = false

  // --- polyhedron optimization ---

  // the following polyhedral optimization levels are currently supported:
  //   0: don't do anything  (fastest; obviously)
  //   1: extract a model and recreate an AST after a polyhedral dead code elimination is performed;
  //        this also allows optimizations of the polyhedral code generator to be applied, as, e.g., avoiding conditional branches inside the loop nest
  //        additionally, a dependence analysis is performed to annotate parallel loops
  //   2: optimize the loop nest by trying to minimze the dependences specified by poly_optimizeDeps
  //   3: tile the optimized loop nest using poly_tileSize_{x|y|z|w}  (slowest)
  // TODO: Alex: range of the following options
  // [0~3§poly_optLevel_fine+1] // poly opt-level for poly_numFinestLevels finest fields
  var poly_optLevel_fine : Int = 0
  // [0~poly_optLevel_fine§poly_optLevel_coarse+1] // polyhedral optimization level for coarsest fields  0: disable (fastest);  3: aggressive (slowest)
  var poly_optLevel_coarse : Int = 0
  // [1~numLevels§poly_numFinestLevels+1] // number of levels that should be optimized in PolyOpt (starting from the finest)
  var poly_numFinestLevels : Int = 2
  // [true|false] // specifies if the polyhedral dead code elimination should be performed (which can increase the generation time)
  var poly_performDCE : Boolean = true

  // for tileSizes: these all default values if they are not set by the performance estimates; '0' means no tiling at all in this dimension
  // TODO: extend for an arbitrary number of dimensions
  // TODO: to be set automatically for each kernel
  // [112~1000000000 $32§poly_tileSize_x+32]
  var poly_tileSize_x : Int = 0
  // [16~1000000000 $32§poly_tileSize_y+32]
  var poly_tileSize_y : Int = 0
  // [16~1000000000 $32§poly_tileSize_z+32]
  var poly_tileSize_z : Int = 0
  // [16~1000000000 $32§poly_tileSize_w+32]
  var poly_tileSize_w : Int = 0

  // [true|false] // triggers an extended exploration (i.e. not only pairs of lines/rays are considered, but also combinations of three of them)
  var poly_exploration_extended : Boolean = false
  // [0~5] // specifies how many (heuristic) filters should be applied to reduce the number or generated schedules during the exploration
  var poly_exploration_filterLevel : Int = 0
  // the following filter levels are currently supported, filter from lower levels are also applied in addition to the one described:
  //   0: no filter
  //   1: only outer loop should carry dependencies (textual dependencies are OK)
  //   2: only linear memory accesses allowed (given that the original schedule had linear accesses)
  //   3: only schedules with textual dependencies (if any)
  //   4: do not prevent aligned vectorization
  //   5: traverse memory in the inner loop in the same direction as the original schedule
  //   6: only positive schedule coefficients allowed
  //   7: only allow schedule coefficients and constants (differences per statement) to be less or equal 2

  // [true|false] // specify separately if the outermost loop should be tiled
  var poly_tileOuterLoop : Boolean = false
  // [isl|feautrier] // choose which schedule algorithm should be used in PolyOpt
  var poly_scheduleAlgorithm : String = "isl"
  // [all|raw|rar] // specifies which dependences should be optimized; "all" means all validity dependences (raw, war, waw)
  var poly_optimizeDeps : String = "raw"
  // [true|false] // specifies if the dependences to optimize should be filtered first (this is a simple heuristics that leads to a better schedule for non-dense Jacobi smoothers)
  var poly_filterDeps : Boolean = false
  // [true|false] // simplify dependences before computing a new schedule; this reduces PolyOpt run-time, but it could also lead to slower generated code
  var poly_simplifyDeps : Boolean = true
  // [true|false] // specifies if parallel composition of schedule components should be sequentialized
  var poly_separateComponents : Boolean = false
  // [true|false] // specifies if the SCCs of the dependence graph are serialized as soon as possible
  var poly_serializeSCCs : Boolean = false
  // [true|false] // split bands as early as possible during schedule generation
  var poly_maximizeBandDepth : Boolean = false
  // [(-1)~inf] // enforces that the constant coefficients in the calculated schedule are not larger than the maximal constant term (this can significantly increase the speed of the scheduling calculation; -1 means unlimited)
  var poly_maxConstantTerm : Int = -1
  // [(-1)~inf] // enforces that the coefficients for variable and parameter dimensions in the calculated schedule are not larger than the specified value (this can significantly increase the speed of the scheduling calculation; -1 means unlimited)
  var poly_maxCoefficient : Int = -1
  var poly_printDebug : Boolean = false

  // a list of external schedules that should be used for the specified ID:
  //  each String must have the form "ID|Schedule"
  var poly_externalSchedules : ListBuffer[String] = ListBuffer()
  var poly_extSched_unrollTime : Boolean = false
  var poly_extSched_outerBandSize : Int = 1

  // a list of IDs for which an exploration should be performed
  //  a "-1" specifies that an exploration should be performed for every scop
  var poly_explorationIDs : ListBuffer[Int] = ListBuffer()

  // --- general optimization ---

  // inlines functions containing less or equal number of statement nodes (0 disables inlining)
  var opt_maxInliningSize : Int = 10

  // [true|false]
  var opt_useAddressPrecalc : Boolean = false

  // unrolling
  // [1~5]
  var opt_unroll : Int = 1
  // [true|false]
  var opt_unroll_interleave : Boolean = false

  // for both CSE: currently assumes RealDatatype for ALL common subexpressions
  // [true|false] // apply a conventional common subexpression elimination
  var opt_conventionalCSE : Boolean = false
  // [true|false] // apply a loop carried common subexpression elimination; this effectively serializes optimized loop-nests, so parallelize LoopOverFragments!
  var opt_loopCarriedCSE : Boolean = false
  // [0~dimensionality] // do not take this number of outer dimensions into account when performing a loop carried CSE
  var opt_loopCarriedCSE_skipOuter : Int = 0
  //for layer conditioning
  var opt_loopBlocked : Boolean = false

  // vectorization

  // [true|false]
  var opt_vectorize : Boolean = false
  var simd_avoidUnaligned : Boolean = false

  /// --- OpenMP ---

  // [true|false]
  var omp_enabled : Boolean = false
  // the number of omp threads to be used; may be incorporated in omp pragmas
  var omp_numThreads : Int = 1

  // [true|false] // specifies if loops over fragments may be parallelized with omp if marked correspondingly
  var omp_parallelizeLoopOverFragments : Boolean = true
  // [true|false] // specifies if loops over dimensions may be parallelized with omp if marked correspondingly
  var omp_parallelizeLoopOverDimensions : Boolean = false
  // [true|false] // if true the 'collapse' directive may be used in omp for regions; this will only be done if the minimum omp version supports this
  var omp_useCollapse : Boolean = false
  // [1~inf§omp_minWorkItemsPerThread+1] // threshold specifying which loops yield enough workload to amortize the omp overhead
  var omp_minWorkItemsPerThread : Int = 400
  // specifies if unique (usually consecutively numbered) identifiers are to be generated for OMP critical sections => allows entering multiple, disctinct sections concurrently
  var omp_nameCriticalSections : Boolean = false

  /// --- MPI ---

  // [true|false]
  var mpi_enabled : Boolean = true
  // the number of mpi threads to be used
  var mpi_numThreads : Int = 1

  // [true|false] // allows to use custom mpi data types when reading from/ writing to fields thus circumventing temp send/ receive buffers
  var mpi_useCustomDatatypes : Boolean = false
  // [true|false] // allows to summarize some code blocks into loops in order to shorten the resulting code length
  var mpi_useLoopsWherePossible : Boolean = true
  // sets the initial communicator used by most MPI operations
  var mpi_defaultCommunicator : String = "MPI_COMM_WORLD"

  // [true|false] // specifies if MPI_Test (true) or MPI_Wait (false) is to be used when waiting for async communication
  var mpi_useBusyWait : Boolean = false

  /// --- CUDA ---

  // [true|false]
  var cuda_enabled : Boolean = false
  // device id of the CUDA device to be used; only relevant in multi-GPU systems
  var cuda_deviceId : String = "0"
  // specifies where kernels should be executed by default; may be "Host", "Device", "Performance" or "Condition"
  var cuda_preferredExecution : String = "Performance"
  // specifies a condition to be used to branch for CPU (true) or GPU (false) execution; only used if cuda_preferredExecution == Condition
  var cuda_executionCondition : String = "true"
  // specifies if CUDA devices are to be synchronized after each (device) kernel call -> recommended to debug, required for reasonable performance measurements
  var cuda_syncDeviceAfterKernelCalls : Boolean = true
  // specifies if fields with (exclusive) write accesses should be synchronized before host kernel executions
  var cuda_syncHostForWrites : Boolean = true
  // specifies if fields with (exclusive) write accesses should be synchronized before device kernel executions
  var cuda_syncDeviceForWrites : Boolean = true

  // default block size in x dimension
  var cuda_blockSize_x : Long = if (dimensionality == 3) 32 else 128
  // default block size in y dimension
  var cuda_blockSize_y : Long = 4
  // default block size in z dimension
  var cuda_blockSize_z : Long = 4

  // the product of the block sizes per dimension, i.e. the total block size
  def cuda_blockSizeTotal : Long = cuda_blockSize_x * cuda_blockSize_y * cuda_blockSize_z
  // shortcut to an array containing the values of cuda_blockSize_*
  def cuda_blockSizeAsVec = Array(cuda_blockSize_x, cuda_blockSize_y, cuda_blockSize_z)

  // default (1D) block size for default reduction kernels
  var cuda_reductionBlockSize = 1024
  // specifies if shared memory should be used within kernels
  var cuda_useSharedMemory : Boolean = false
  // TODO
  var cuda_linearizeSharedMemoryAccess : Boolean = false
  // TODO
  var cuda_spatialBlockingWithSmem : Boolean = false
  // TODO
  var cuda_favorL1CacheOverSharedMemory : Boolean = false
  // apply spatial blocking with read-only cache
  var cuda_spatialBlockingWithROC : Boolean = false

  // if true, the first dimension of the block size is enlarged if the kernel dimensionality is lower than the global dimensionality
  var cuda_foldBlockSizeForRedDimensionality : Boolean = true

  /// --- general parallelization ---

  // specifies if DefaultNeighbors is set up with only axis neighbors (4 in 2D and 6 in 3D) or diagonal neighbors as well
  var comm_onlyAxisNeighbors : Boolean = true
  // specifies if data from ghost layers are to be communicated as well when sending data from an edge or a face
  var comm_syncGhostData : Boolean = true
  // specifies if communication should be batched; current default is pairs of two
  var comm_batchCommunication : Boolean = true
  // specifies if index transformations in communication are used (required for block-structured grids)
  var comm_enableCommTransformations : Boolean = false

  // [0|6|26] // keep for backwards compatibility; originally used to specify if communication is only performed along coordinate axis or to all neighbors
  var comm_strategyFragment : Int = 0

  // [true|false] // specifies if comm ops (buffer copy, send/ recv, wait) should each be aggregated and handled in distinct fragment loops
  var comm_useFragmentLoopsForEachOp : Boolean = true
  // [true|false] // specifies if local data exchanges are implemented using push (true) or pull (false) schemes
  var comm_pushLocalData : Boolean = false
  // [true|false] // specifies if local communication is synchronized using flags; usually not necessary unless communication in fragment loops is allowed
  var comm_disableLocalCommSync = true

  // TODO: check in how far the following parameters can be adapted by the SPL
  // specifies if communication variables that could be fragment specific are handled separately
  var comm_sepDataByFragment : Boolean = true
  // specifies if communication variables that could be domain specific are handled separately
  var comm_sepDataByDomain : Boolean = false
  // specifies if communication variables that could be field specific are handled separately
  var comm_sepDataByField : Boolean = false
  // specifies if communication variables that could be level specific are handled separately
  var comm_sepDataByLevel : Boolean = false
  // specifies if communication variables that could be neighbor specific are handled separately
  var comm_sepDataByNeighbor : Boolean = true

  // specifies if fragment specific variables are summarized in array form
  var comm_useFragmentArrays : Boolean = true
  // specifies if domain specific variables are summarized in array form
  var comm_useDomainArrays : Boolean = true
  // specifies if domain field variables are summarized in array form
  var comm_useFieldArrays : Boolean = false
  // specifies if domain level variables are summarized in array form
  var comm_useLevelArrays : Boolean = false
  // specifies if neighbor specific variables are summarized in array form
  var comm_useNeighborArrays : Boolean = true

  /// performance estimation

  // adds an optimistic performance estimation according to a roofline model
  var performance_addEstimation : Boolean = false

  // prints estimated performance to file (cf Settings.performanceEstimateOutputFile)
  var performance_printEstimation : Boolean = false

  /// --- temporary flags ---

  /// experimental features

  // generates call stacks for all employed timers
  var experimental_disableIterationOffsets : Boolean = false
  var experimental_bc_checkOnlyMainAxis : Boolean = true
  var experimental_bc_avoidOrOperations : Boolean = true

  var experimental_forceOmitCondInLocalSolve : Boolean = false

  // use this flag to resolve iteration offsets -> use only for single fragments!
  var experimental_useStefanOffsets : Boolean = false

  var experimental_resolveUnreqFragmentLoops : Boolean = false

  // eliminates conditions of the form IR_IfCondition(IR_IntegerConstant(_), _, _)
  var experimental_emliminateIntConditions : Boolean = false

  var experimental_allowCommInFragLoops : Boolean = false

  var experimental_generateParaviewFiles : Boolean = false

  var experimental_trimBoundsForReductionLoops : Boolean = true

  // tries to merge communication statements and loop over points in function bodies -> allows automatic overlap of communication and computation
  var experimental_mergeCommIntoLoops : Boolean = false
  // attempts to overlap communication and computation of loops with added communication statements
  var experimental_splitLoopsForAsyncComm : Boolean = false
  // minimum width of inner dimension when splitting according to experimental_splitLoopsForAsyncComm; 0 to disable
  var experimental_splitLoops_minInnerWidth : Int = 4

  var experimental_resolveInverseFunctionCall : String = "Cofactors" // [Cofactors|GaussJordan|Runtime] // how to resolve inverse() function calls for matrices:

  // tries to apply an inversion based on the Schur complement in local solve blocks
  var experimental_applySchurCompl : Boolean = false

  // eliminate occurrences of cudaContext - required for PizDaint
  var experimental_eliminateCudaContext : Boolean = false

  // generate cuda kernels independently of them being parallel or not
  var experimental_cuda_generateKernelForNonParallel : Boolean = false

  // control inlining of value declarations on applicable layers
  var experimental_l1_inlineValueDeclarations : Boolean = false
  var experimental_l2_inlineValueDeclarations : Boolean = false
  var experimental_l3_inlineValueDeclarations : Boolean = false
  var experimental_l4_inlineValueDeclarations : Boolean = true

  var experimental_l2_resolveVirtualFields : Boolean = true
  var experimental_l3_resolveVirtualFields : Boolean = true
  var experimental_l4_resolveVirtualFields : Boolean = true

  // specifies if all levels should be read from file or just the max-level
  var experimental_domain_readAllLevelsFromFile : Boolean = false

  var experimental_grid_randomMaxOffset : Double = 0.1

  /// paper project - SISC

  // [16~64§sisc2015_numNodes*2]
  var sisc2015_numNodes : Int = 64
  // [16~64§sisc2015_ranksPerNode*2]
  var sisc2015_ranksPerNode : Int = 64
  // [0~1]
  var sisc2015_firstDim : Int = 1
  // [0~1]
  var sisc2015_secondDim : Int = 1

  // [1~64§sisc2015_numOMP_x*2]
  var sisc2015_numOMP_x : Int = 2
  // [1~64§sisc2015_numOMP_y*2]
  var sisc2015_numOMP_y : Int = 2
  // [1~64§sisc2015_numOMP_z*2]
  var sisc2015_numOMP_z : Int = 2

  /// student project - Richard / visit

  // TODO
  var experimental_visit_enable : Boolean = false

  /// END HACK

  /// === constraints and resolutions ===
  def update() : Unit = {
    // NOTE: it is required to call update at least once

    Constraints.condEnsureValue(Platform.targetCompilerVersion, 11, "MSVC" == Platform.targetCompiler && Platform.targetCompilerVersion < 11, "When using MSVC, only versions > 11.0 are currently supported")
    Constraints.condEnsureValue(Platform.targetCompilerVersion, 14, "MSVC" == Platform.targetCompiler && Platform.targetCompilerVersion > 14, "When using MSVC, only version up to 14.0 are currently supported")
    Constraints.condEnsureValue(Platform.targetCompilerVersionMinor, 0, "MSVC" == Platform.targetCompiler, "When using MSVC, minor version numbers are not supported")

    Constraints.condEnsureValue(omp_enabled, false, "CLANG" == Platform.targetCompiler && (Platform.targetCompilerVersion * 10 + Platform.targetCompilerVersionMinor < 37), "Only clang >= 3.7 supports OpenMP")

    Constraints.condEnsureValue(opt_vectorize, false, "GCC" == Platform.targetCompiler && "IMCI" == Platform.simd_instructionSet, "GCC does not support intel IMCI")
    Constraints.condEnsureValue(Platform.simd_instructionSet, "QPX", "IBMBG" == Platform.targetCompiler && opt_vectorize, "IBM BlueGene/Q compiler can only generate code for BlueGene/Q (with vector extension QPX)")
    Constraints.condEnsureValue(opt_vectorize, false, "IBMBG" != Platform.targetCompiler && "QPX" == Platform.simd_instructionSet, "only IBM BlueGene/Q compiler supports QPX")

    // domain
    Constraints.condEnsureValue(domain_rect_generate, false, !domain_onlyRectangular, "only rectangular domains can be generated")
    Constraints.condEnsureValue(domain_readFromFile, true, !domain_rect_generate && !domain_onlyRectangular, "non-generated domains must be read from file")
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
    Constraints.condEnsureValue(maxLevel, 0, maxLevel < 0, "maxLevel must not be negative")
    // Constraints.condEnsureValue(minLevel, maxLevel - 1, minLevel >= maxLevel, "maxLevel must be larger than minLevel") // TODO: this seems unnecessary -> check if sth breaks

    // grid
    Constraints.condEnsureValue(grid_spacingModel, "uniform", grid_isUniform, "uniform spacing is required for uniform grids")
    Constraints.condWarn("uniform" == grid_spacingModel && !grid_isUniform, "grid_isUniform should be true for uniform spacing models")
    Constraints.condWarn("diego" == grid_spacingModel, "diego spacing model currently ignores domain bounds set in the DSL")
    Constraints.condWarn("diego2" == grid_spacingModel, "diego2 spacing model currently ignores domain bounds set in the DSL")
    Constraints.condWarn("blockstructured" == grid_spacingModel && grid_isUniform, "grid_isUniform should be false for block-structured grids")
    Constraints.condWarn(grid_halveStagBoundaryVolumes && "uniform" == grid_spacingModel, "halving staggered volumes at the boundary is not supported for uniform grids")

    // backwards compatibility for comm_strategyFragment
    Constraints.condWarn(comm_strategyFragment != 0, "comm_strategyFragment is deprecated and will be removed in the future")
    Constraints.condEnsureValue(comm_onlyAxisNeighbors, true, 6 == comm_strategyFragment, "comm_onlyAxisNeighbors must match comm_strategyFragment")
    Constraints.condEnsureValue(comm_onlyAxisNeighbors, false, 26 == comm_strategyFragment, "comm_onlyAxisNeighbors must match comm_strategyFragment")
    Constraints.condEnsureValue(comm_syncGhostData, true, 6 == comm_strategyFragment, "comm_syncGhostData must match comm_strategyFragment")
    Constraints.condEnsureValue(comm_syncGhostData, false, 26 == comm_strategyFragment, "comm_syncGhostData must match comm_strategyFragment")
    Constraints.condEnsureValue(comm_batchCommunication, true, 6 == comm_strategyFragment, "comm_batchCommunication must match comm_strategyFragment")
    Constraints.condEnsureValue(comm_batchCommunication, false, 26 == comm_strategyFragment, "comm_batchCommunication must match comm_strategyFragment")

    // comm_enableCommTransformations must be true when domain_readFromFile is true
    Constraints.condEnsureValue(comm_enableCommTransformations, true, domain_readFromFile, "comm_enableCommTransformations only works on block-structured grids, domain_readFromFile is set to true.")
    Constraints.condEnsureValue(comm_enableCommTransformations, false, !domain_readFromFile, "comm_enableCommTransformations only works on block-structured grids.")

    // parallelization
    Constraints.condEnsureValue(mpi_numThreads, 1, !mpi_enabled, "Setting mpi_numThreads to 1 since mpi is deactivated")
    Constraints.condError(domain_numBlocks > 1 && !mpi_enabled, "For the moment, configurations with more then one block must use MPI")

    Constraints.condEnsureValue(omp_useCollapse, false, "IBMXL" == Platform.targetCompiler || "IBMBG" == Platform.targetCompiler, "omp collapse is currently not fully supported by the IBM XL compiler")
    Constraints.condEnsureValue(omp_parallelizeLoopOverDimensions, false, omp_enabled && omp_parallelizeLoopOverFragments, "omp_parallelizeLoopOverDimensions and omp_parallelizeLoopOverFragments are mutually exclusive")

    Constraints.condWarn(mpi_numThreads != domain_numBlocks, s"the number of mpi threads ($mpi_numThreads) differs from the number of blocks ($domain_numBlocks) -> this might lead to unexpected behavior!")
    Constraints.condEnsureValue(omp_enabled, false, omp_numThreads == 1, s"The number of omp threads is equal to one, but omp_enabled is true")
    Constraints.condEnsureValue(omp_numThreads, 1, !omp_enabled, s"The number of omp threads is larger than one ($omp_numThreads), but omp_enabled is false")
    Constraints.condWarn(omp_parallelizeLoopOverFragments && omp_numThreads > domain_numFragmentsPerBlock, s"the number of omp threads ($omp_numThreads) is higher than the number of fragments per block ($domain_numFragmentsPerBlock) -> this will result in idle omp threads!")
    Constraints.condWarn(omp_parallelizeLoopOverFragments && 0 != domain_numFragmentsPerBlock % omp_numThreads, s"the number of fragments per block ($domain_numFragmentsPerBlock) is not divisible by the number of omp threads ($omp_numThreads) -> this might result in a severe load imbalance!")
    Constraints.condWarn(omp_nameCriticalSections, s"omp_nameCriticalSections should always be deactivated")

    Constraints.condWarn(experimental_allowCommInFragLoops && omp_numThreads != domain_numFragmentsPerBlock, s"It is strongly recommended that the number of omp threads ($omp_numThreads) is equal to the number of fragments per block ($domain_numFragmentsPerBlock) when experimental_allowCommInFragLoops is enabled!")

    Constraints.condEnsureValue(mpi_useCustomDatatypes, false, data_genVariableFieldSizes, "MPI data types cannot be used in combination with variable field sizes yet")

    Constraints.condEnsureValue(mpi_useBusyWait, true, experimental_allowCommInFragLoops && domain_numFragmentsPerBlock > 1, s"mpi_useBusyWait must be true when experimental_allowCommInFragLoops is used in conjunction with multiple fragments per block")
    Constraints.condWarn(comm_disableLocalCommSync && experimental_allowCommInFragLoops, s"comm_disableLocalCommSynchronization in conjunction with experimental_allowCommInFragLoops is strongly discouraged")

    Constraints.condEnsureValue(performance_addEstimation, true, cuda_enabled && "Performance" == cuda_preferredExecution, s"experimental_addPerformanceEstimate is required for performance estimate guided kernel execution")
    Constraints.condEnsureValue(cuda_deviceId, "0", cuda_enabled && cuda_deviceId.forall(_.isDigit) && cuda_deviceId.toInt >= Platform.hw_gpu_numDevices, s"CUDA device id must not be exceeding number of installed devices")

    Constraints.condEnsureValue(cuda_blockSize_y, 1, cuda_enabled && domain_rect_generate && dimensionality < 2, "experimental_cuda_blockSize_y must be set to 1 for problems with a dimensionality smaller 2")
    Constraints.condEnsureValue(cuda_blockSize_z, 1, cuda_enabled && domain_rect_generate && dimensionality < 3, "experimental_cuda_blockSize_z must be set to 1 for problems with a dimensionality smaller 3")

    Constraints.condWarn(cuda_enabled && cuda_blockSizeTotal > 512 && Platform.hw_cuda_capability <= 2, s"CUDA block size has been set to $cuda_blockSizeTotal, this is not supported by compute capability ${ Platform.hw_cuda_capability }.${ Platform.hw_cuda_capabilityMinor }")
    Constraints.condWarn(cuda_enabled && cuda_blockSizeTotal > 1024 && Platform.hw_cuda_capability >= 3, s"CUDA block size has been set to $cuda_blockSizeTotal, this is not supported by compute capability ${ Platform.hw_cuda_capability }.${ Platform.hw_cuda_capabilityMinor }")

    Constraints.condWarn(cuda_useSharedMemory && cuda_favorL1CacheOverSharedMemory, "If CUDA shared memory usage is enabled, it is not very useful to favor L1 cache over shared memory storage!")
    Constraints.condWarn(cuda_spatialBlockingWithSmem && !cuda_useSharedMemory, "Spatial blocking with shared memory can only be used if shared memory usage is enabled!")
    Constraints.condEnsureValue(cuda_spatialBlockingWithSmem, false, !cuda_useSharedMemory)
    Constraints.condWarn((cuda_useSharedMemory || cuda_spatialBlockingWithSmem) && cuda_spatialBlockingWithROC, "Shared memory and/or spatial blocking with shared memory cannot be used if spatial blocking with read-only cache is enabled!")
    Constraints.condEnsureValue(cuda_useSharedMemory, false, cuda_spatialBlockingWithROC)
    Constraints.condEnsureValue(cuda_spatialBlockingWithSmem, false, cuda_spatialBlockingWithROC)
    Constraints.condWarn(cuda_enabled && poly_performDCE, "Polyhedral dead code elimination cannot be performed if CUDA is used!")
    Constraints.condEnsureValue(poly_performDCE, false, cuda_enabled)
    Constraints.condWarn(cuda_enabled && opt_loopCarriedCSE, "Loop carried CSE cannot be applied if CUDA is used!")
    Constraints.condEnsureValue(opt_loopCarriedCSE, false, cuda_enabled)

    Constraints.condWarn(cuda_enabled && opt_conventionalCSE && !useDblPrecision, "Double precision should be used if CUDA is enabled and CSE should be applied!")
    Constraints.condEnsureValue(useDblPrecision, true, cuda_enabled && opt_conventionalCSE)

    Constraints.condWarn(experimental_splitLoopsForAsyncComm && !comm_onlyAxisNeighbors, s"Using asynchronous communication with comm_onlyAxisNeighbors leads to problems with stencils containing diagonal entries")

    // data
    Constraints.condEnsureValue(data_alignFieldPointers, true, opt_vectorize && "QPX" == Platform.simd_instructionSet, "data_alignFieldPointers must be true for vectorization with QPX")

    Constraints.condEnsureValue(simd_avoidUnaligned, true, opt_vectorize && "QPX" == Platform.simd_instructionSet, "QPX does not support unaligned loads/stores")
    Constraints.condEnsureValue(simd_avoidUnaligned, true, opt_vectorize && "IMCI" == Platform.simd_instructionSet, "IMCI does not support unaligned loads/stores")
    Constraints.condEnsureValue(simd_avoidUnaligned, false, !opt_vectorize, "avoid unaligned loads/stores doesn't make sense without vectorization enabled")
    Constraints.condEnsureValue(simd_avoidUnaligned, false, !data_alignFieldPointers, "impossible to avoid unaligned accesses if data is not aligned")

    // optimization
    Constraints.condEnsureValue(poly_optLevel_coarse, poly_optLevel_fine, poly_optLevel_coarse > poly_optLevel_fine, "optimization level for coarse grids must smaller or equal to the one for the fine levels")
    Constraints.condEnsureValue(poly_numFinestLevels, numLevels, poly_numFinestLevels > numLevels, "number of fine levels (for optimization) cannot exceed the number of all levels")
    Constraints.condEnsureValue(poly_maximizeBandDepth, true, poly_serializeSCCs, "poly_maximizeBandDepth has no effect if poly_serializeSCCs is set")

    Constraints.condEnsureValue(opt_loopCarriedCSE_skipOuter, 0, !opt_loopCarriedCSE, "loop-carried cse disbaled, set its configuration parameter to default")
    // TODO: is it worth the effort to fix the following?
    Constraints.condWarn(omp_enabled && omp_numThreads > 1 && opt_loopCarriedCSE_skipOuter > 0 && poly_optLevel_fine > 0, "skipping outer loops fpr loop-carried cse in combination with PolyOpt may currently prevent OpenMP parallelism and vectorization")

    // timer configuration
    Constraints.condEnsureValue(timer_type, "Chrono", !mpi_enabled && "MPI_TIME" == timer_type, "MPI_TIME is not supported for codes generated without MPI")
    Constraints.condEnsureValue(timer_type, "Chrono", "QPC" == timer_type && "MSVC" != Platform.targetCompiler, "QPC is only supported for windows")
    Constraints.condEnsureValue(timer_type, "WINDOWS_RDSC", "RDSC" == timer_type && "MSVC" == Platform.targetCompiler, "WINDOWS_RDSC is required for windows systems")
    Constraints.condEnsureValue(timer_type, "RDSC", "WINDOWS_RDSC" == timer_type && "MSVC" != Platform.targetCompiler, "RDSC is required for non-windows systems")
    Constraints.condEnsureValue(timer_type, "UNIX_TIME", "WIN_TIME" == timer_type && "MSVC" != Platform.targetCompiler, "WIN_TIME is not supported for non-windows systems")
    Constraints.condEnsureValue(timer_type, "WIN_TIME", "UNIX_TIME" == timer_type && "MSVC" == Platform.targetCompiler, "UNIX_TIME is not supported for windows systems")
    Constraints.condEnsureValue(timer_type, "UNIX_TIME", "Chrono" == timer_type && "IBMXL" == Platform.targetCompiler, "IBM XL does currently not support std::chrono")
    Constraints.condEnsureValue(timer_type, "UNIX_TIME", "Chrono" == timer_type && "IBMBG" == Platform.targetCompiler, "IBM BG does currently not support std::chrono")

    // benchmarking and performance estimation

    Constraints.condWarn(!List("None", "likwid").contains(benchmark_backend), "Unknown value for benchmark_backend")
    Constraints.condError(benchmark_backend == "likwid" && Platform.targetOS != "Linux", "likwid is currently only available for Linux")

    Constraints.condEnsureValue(performance_addEstimation, true, performance_printEstimation, "printing performance estimations requires actually estimating them")
    Constraints.condEnsureValue(performance_addEstimation, true, opt_loopBlocked, "loop blocking requires setting up a performance model")

    // experimental
    Constraints.condEnsureValue(experimental_trimBoundsForReductionLoops, false, data_genVariableFieldSizes, "experimental_trimBoundsForReductionLoops is currently not compatible with data_genVariableFieldSizes")
    Constraints.condEnsureValue(experimental_useStefanOffsets, false, domain_numFragmentsTotal > 1, "experimental_useStefanOffsets requires a single fragment")
  }
}
