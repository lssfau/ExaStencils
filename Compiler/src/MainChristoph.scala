import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.baseExt.l4._
import exastencils.boundary.ir.L4_ResolveBoundaryHandlingFunctions
import exastencils.communication.IR_LinearizeTempBufferAccess
import exastencils.communication.ir._
import exastencils.config._
import exastencils.core._
import exastencils.cuda._
import exastencils.datastructures._
import exastencils.deprecated.ir.CreateGeomCoordinates
import exastencils.deprecated.l3Generate
import exastencils.domain.{ l4 => _, _ }
import exastencils.field.ir._
import exastencils.globals._
import exastencils.globals.ir._
import exastencils.grid.{ l4 => _, _ }
import exastencils.interfacing.ir._
import exastencils.knowledge.l4._
import exastencils.knowledge.{ l4 => _, _ }
import exastencils.logger._
import exastencils.mpi.ir._
import exastencils.multiGrid._
import exastencils.omp.ir._
import exastencils.optimization._
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.parsers.l4._
import exastencils.parsers.settings._
import exastencils.performance._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.stencil.ir.IR_ResolveStencilFunction
import exastencils.stencil.l4.L4_ProcessStencilDeclarations
import exastencils.util._
import exastencils.util.l4.L4_ResolveSpecialConstants

object MainChristoph {
  private var polyOptExplID : Int = 0

  def initialize(args : Array[String]) : Unit = {
    //if (Settings.timeStrategies) -> right now this Schroedinger flag is neither true nor false
    StrategyTimer.startTiming("Initializing")

    // check from where to read input
    val settingsParser = new ParserSettings
    val knowledgeParser = new ParserKnowledge
    val platformParser = new ParserPlatform
    if (args.length == 1 && args(0) == "--json-stdin") {
      InputReader.read()
      settingsParser.parse(InputReader.settings)
      if (Settings.produceHtmlLog) Logger_HTML.init // allows emitting errors and warning in knowledge and platform parsers
      knowledgeParser.parse(InputReader.knowledge)
      platformParser.parse(InputReader.platform)
      Knowledge.l3tmp_generateL4 = false // No Layer4 generation with input via JSON
    } else if (args.length == 2 && args(0) == "--json-file") {
      InputReader.read(args(1))
      settingsParser.parse(InputReader.settings)
      if (Settings.produceHtmlLog) Logger_HTML.init // allows emitting errors and warning in knowledge and platform parsers
      knowledgeParser.parse(InputReader.knowledge)
      platformParser.parse(InputReader.platform)
      Knowledge.l3tmp_generateL4 = false // No Layer4 generation with input via JSON
    } else {
      if (args.length >= 1)
        settingsParser.parseFile(args(0))
      if (Settings.produceHtmlLog) Logger_HTML.init // allows emitting errors and warning in knowledge and platform parsers
      if (args.length >= 2)
        knowledgeParser.parseFile(args(1))
      if (args.length >= 3)
        platformParser.parseFile(args(2))
      if (args.length >= 4)
        polyOptExplID = args(3).toInt
    }

    // validate knowledge, etc.
    Knowledge.update()
    Settings.update()
    Platform.update()

    if (Settings.cancelIfOutFolderExists) {
      if (new java.io.File(Settings.getOutputPath).exists) {
        Logger.error(s"Output path ${ Settings.getOutputPath } already exists but cancelIfOutFolderExists is set to true. Shutting down now...")
        sys.exit(0)
      }
    }

    // init buildfile generator, overrides settings file
    if ("MSVC" == Platform.targetCompiler)
      Settings.buildfileGenerators = ListBuffer("ProjectfileGenerator")

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Initializing")
  }

  def shutdown() : Unit = {
    if (Settings.timeStrategies)
      StrategyTimer.print

    if (Settings.produceHtmlLog)
      Logger_HTML.finish
  }

  def handleL1() : Unit = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 1")

    // add L1 code here

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 1")
  }

  def handleL2() : Unit = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 2")

    /// HACK: This information has to come from L2
    if (Knowledge.domain_rect_generate) {
      Knowledge.discr_hx = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
        level => l3Generate.Domains.getGlobalWidths(0) / (Knowledge.domain_rect_numFragsTotal_x * Knowledge.domain_fragmentLength_x * (1 << level)))
      if (Knowledge.dimensionality > 1)
        Knowledge.discr_hy = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
          level => l3Generate.Domains.getGlobalWidths(1) / (Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_fragmentLength_y * (1 << level)))
      if (Knowledge.dimensionality > 2)
        Knowledge.discr_hz = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
          level => l3Generate.Domains.getGlobalWidths(2) / (Knowledge.domain_rect_numFragsTotal_z * Knowledge.domain_fragmentLength_z * (1 << level)))
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 2")
  }

  def handleL3() : Unit = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 3")

    // Looking for other L3 related code? Check MainL3.scala!

    if (Knowledge.l3tmp_generateL4) {
      StateManager.root_ = l3Generate.Root()
      StateManager.root_.asInstanceOf[l3Generate.Root].printToL4(Settings.getL4file)
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 3")
  }

  def handleL4() : Unit = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 4")

    if (Settings.inputFromJson) {
      StateManager.root_ = (new ParserL4).parseFile(InputReader.layer4)
    } else {
      StateManager.root_ = (new ParserL4).parseFile(Settings.getL4file)
    }
    ValidationL4.apply()

    if (false) // re-print the merged L4 state
    {
      val L4_printed = StateManager.root_.asInstanceOf[L4_Root].prettyprint()

      val outFile = new java.io.FileWriter(Settings.getL4file + "_rep.exa")
      outFile.write(Indenter.addIndentations(L4_printed))
      outFile.close()

      // re-parse the file to check for errors
      val parserl4 = new ParserL4
      StateManager.root_ = parserl4.parseFile(Settings.getL4file + "_rep.exa")
      ValidationL4.apply()
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 4")

    // add specialized fields for geometric data - TODO: decide if better left here or moved to ir
    GridGeometry.getGeometry.initL4()

    // go to IR
    L4_ResolveFunctionInstantiations.apply()

    L4_ResolveLevelSpecifications.apply()

    L4_UnfoldLeveledFunctions.apply()
    L4_UnfoldLeveledDeclarations.apply()
    L4_UnfoldLeveledKnowledgeDecls.apply()
    L4_ResolveLeveledScopes.apply()

    L4_ResolveCurrentLevels.apply()
    L4_ResolveSpecialConstants.apply()
    L4_ResolveKnowledgeParameterAccess.apply()

    L4_ProcessStencilDeclarations.apply()

    L4_ResolveBoundaryHandlingFunctions.apply()

    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Progressing from L4 to IR")

    StateManager.root_ = StateManager.root_.asInstanceOf[L4_Progressable].progress.asInstanceOf[Node]

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Progressing from L4 to IR")
  }

  def handleIR() : Unit = {
    // add some more nodes
    AddDefaultGlobals.apply()

    Fragment.setupNeighbors()
    StateManager.findFirst[Globals]().get.functions += IR_AllocateDataFunction(IR_FieldCollection.objects, Fragment.neighbors)
    StateManager.findFirst[MultiGridFunctions]().get.functions ++= IR_ExternalFieldCollection.generateCopyFunction()

    // add remaining nodes
    StateManager.root_.asInstanceOf[IR_Root].nodes ++= List(
      // FunctionCollections
      DomainFunctions(),
      IR_CommunicationFunctions(),

      // Util
      Stopwatch(),
      TimerFunctions(),
      Matrix(), // TODO: only if required
      CImg() // TODO: only if required
    )

    if (Knowledge.cuda_enabled)
      StateManager.root_.asInstanceOf[IR_Root].nodes += KernelFunctions()

    if (Knowledge.experimental_mergeCommIntoLoops)
      IR_MergeCommunicateAndLoop.apply()
    IR_GeneralSimplify.doUntilDone() // removes (conditional) calls to communication functions that are not possible
    IR_SetupCommunication.firstCall = true
    IR_SetupCommunication.apply()

    ResolveSpecialFunctionsAndConstants.apply()

    IR_ResolveLoopOverPoints.apply()
    ResolveIntergridIndices.apply()

    var convChanged = false
    do {
      FindStencilConvolutions.changed = false
      FindStencilConvolutions.apply()
      convChanged = FindStencilConvolutions.changed
      if (Knowledge.useFasterExpand)
        IR_ExpandInOnePass.apply()
      else
        IR_Expand.doUntilDone()
    } while (convChanged)

    IR_ResolveStencilFunction.apply()
    Grid.applyStrategies()
    if (Knowledge.domain_fragmentTransformation) CreateGeomCoordinates.apply() // TODO: remove after successful integration

    IR_ResolveLoopOverPointsInOneFragment.apply()

    IR_SetupCommunication.apply() // handle communication statements generated by loop resolution

    TypeInference.warnMissingDeclarations = false
    TypeInference.apply() // first sweep to allow for VariableAccess extraction in SplitLoopsForHostAndDevice

    if (Knowledge.experimental_addPerformanceEstimate)
      AddPerformanceEstimates()
    // Prepare all suitable LoopOverDimensions and ContractingLoops. This transformation is applied before resolving
    // ContractingLoops to guarantee that memory transfer statements appear only before and after a resolved
    // ContractingLoop (required for temporal blocking). Leads to better device memory occupancy.
    if (Knowledge.cuda_enabled) {
      PrepareCudaRelevantCode.apply()
    }

    IR_ResolveContractingLoop.apply()

    IR_SetupCommunication.apply() // handle communication statements generated by loop resolution

    MapStencilAssignments.apply()
    IR_ResolveFieldAccess.apply()

    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()

    // resolve constant IVs before applying poly opt
    IR_ResolveConstIVs.apply()
    IR_GeneralSimplify.doUntilDone()

    if (Knowledge.opt_conventionalCSE || Knowledge.opt_loopCarriedCSE) {
      new DuplicateNodes().apply() // FIXME: only debug
      Inlining.apply(true)
      CommonSubexpressionElimination.apply()
    }

    MergeConditions.apply()
    if (Knowledge.poly_optLevel_fine > 0)
      PolyOpt.apply(polyOptExplID)
    IR_ResolveLoopOverDimensions.apply()

    TypeInference.apply() // second sweep for any newly introduced nodes - TODO: check if this is necessary

    // Apply CUDA kernel extraction after polyhedral optimizations to work on optimized ForLoopStatements
    if (Knowledge.cuda_enabled) {
      CalculateCudaLoopsAnnotations.apply()
      ExtractHostAndDeviceCode.apply()
      AdaptKernelDimensionalities.apply()
      HandleKernelReductions.apply()
    }

    if (Knowledge.opt_useColorSplitting)
      ColorSplitting.apply()

    // before converting kernel functions -> requires linearized accesses
    IR_LinearizeDirectFieldAccess.apply()
    IR_LinearizeExternalFieldAccess.apply()
    IR_LinearizeTempBufferAccess.apply()
    IR_LinearizeReductionDeviceDataAccess.apply()
    IR_LinearizeLoopCarriedCSBufferAccess.apply()

    if (Knowledge.cuda_enabled)
      StateManager.findFirst[KernelFunctions]().get.convertToFunctions()

    IR_ResolveBoundedScalar.apply() // after converting kernel functions -> relies on (unresolved) index offsets to determine loop iteration counts
    IR_ResolveSlotOperations.apply() // after converting kernel functions -> relies on (unresolved) slot accesses

    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()

    if (!Knowledge.mpi_enabled)
      MPI_RemoveMPI.apply()

    IR_GeneralSimplify.doUntilDone()

    if (Knowledge.opt_useAddressPrecalc)
      AddressPrecalculation.apply()

    SimplifyFloatExpressions.apply()

    if (Knowledge.opt_vectorize)
      Vectorization.apply()

    if (Knowledge.opt_unroll > 1)
      Unrolling.apply()

    if (Knowledge.opt_vectorize)
      RemoveDupSIMDLoads.apply()

    if (Knowledge.data_genVariableFieldSizes)
      IR_GenerateIndexManipFcts.apply()
    IR_AddInternalVariables.apply()
    // resolve possibly newly added constant IVs
    IR_ResolveConstIVs.apply()

    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()

    if (Knowledge.mpi_enabled)
      MPI_AddDatatypeSetup.apply()

    if (Knowledge.omp_enabled) {
      OMP_AddParallelSections.apply()

      // resolve min/max reductions if omp version does not support them inherently
      if (Platform.omp_version < 3.1)
        OMP_ResolveMinMaxReduction.apply()

      if (Platform.omp_requiresCriticalSections)
        OMP_AddCriticalSections.apply()
    }

    // one last time
    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()
    IR_GeneralSimplify.doUntilDone()

    if (Knowledge.opt_maxInliningSize > 0)
      Inlining.apply()

    if (Knowledge.generateFortranInterface)
      IR_Fortranify.apply()
  }

  def print() : Unit = {
    Logger.dbg("Prettyprinting to folder " + new java.io.File(Settings.getOutputPath).getAbsolutePath)
    PrintToFile.apply()
    PrettyprintingManager.finish
  }

  def main(args : Array[String]) : Unit = {
    // for runtime measurement
    val start : Long = System.nanoTime()

    initialize(args)

    handleL1()
    handleL2()
    handleL3()
    handleL4()
    handleIR()

    print()

    Logger.dbg("Done!")

    Logger.dbg("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
    new CountingStrategy("number of printed nodes").apply()

    shutdown()
  }
}
