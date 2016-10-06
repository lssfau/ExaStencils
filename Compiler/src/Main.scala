import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Root
import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.boundary.ir.L4_ResolveBoundaryHandlingFunctions
import exastencils.communication.ir._
import exastencils.core._
import exastencils.cuda._
import exastencils.data._
import exastencils.datastructures._
import exastencils.deprecated.ir.CreateGeomCoordinates
import exastencils.deprecated.l3Generate
import exastencils.domain.{ l4 => _, _ }
import exastencils.field.ir._
import exastencils.field.l4._
import exastencils.globals._
import exastencils.grid.l4._
import exastencils.grid.{ l4 => _, _ }
import exastencils.hack.l4.HACK_L4_ResolveNativeFunctions
import exastencils.knowledge.l4._
import exastencils.knowledge.{ l4 => _, _ }
import exastencils.logger._
import exastencils.mpi.ir._
import exastencils.multiGrid._
import exastencils.omp._
import exastencils.omp.ir._
import exastencils.optimization._
import exastencils.parsers.l4._
import exastencils.parsers.settings._
import exastencils.performance._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.solver.ir.IR_ResolveLocalSolve
import exastencils.stencil.l4._
import exastencils.strategies._
import exastencils.timing.ir.IR_AdaptTimerFunctions
import exastencils.timing.l4.L4_ResolveTimerFunctions
import exastencils.util._
import exastencils.util.l4._

object Main {
  def initialize(args : Array[String]) = {
    //if (Settings.timeStrategies) -> right now this Schroedinger flag is neither true nor false
    StrategyTimer.startTiming("Initializing")

    // check from where to read input
    val settingsParser = new ParserSettings()
    val knowledgeParser = new ParserKnowledge()
    val platformParser = new ParserPlatform()
    if (args.length == 1 && args(0) == "--json-stdin") {
      InputReader.read()
      settingsParser.parse(InputReader.settings)
      if (Settings.produceHtmlLog) Logger_HTML.init() // allows emitting errors and warning in knowledge and platform parsers
      knowledgeParser.parse(InputReader.knowledge)
      platformParser.parse(InputReader.platform)
      Knowledge.l3tmp_generateL4 = false // No Layer4 generation with input via JSON
    } else if (args.length == 2 && args(0) == "--json-file") {
      InputReader.read(args(1))
      settingsParser.parse(InputReader.settings)
      if (Settings.produceHtmlLog) Logger_HTML.init() // allows emitting errors and warning in knowledge and platform parsers
      knowledgeParser.parse(InputReader.knowledge)
      platformParser.parse(InputReader.platform)
      Knowledge.l3tmp_generateL4 = false // No Layer4 generation with input via JSON
    } else {
      if (args.length >= 1)
        settingsParser.parseFile(args(0))
      if (Settings.produceHtmlLog) Logger_HTML.init() // allows emitting errors and warning in knowledge and platform parsers
      if (args.length >= 2)
        knowledgeParser.parseFile(args(1))
      if (args.length >= 3)
        platformParser.parseFile(args(2))
    }

    // validate knowledge, etc.
    Knowledge.update()
    Settings.update()
    Platform.update()

    if (Settings.cancelIfOutFolderExists) {
      if ((new java.io.File(Settings.getOutputPath)).exists()) {
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

  def shutdown() = {
    if (Settings.timeStrategies)
      StrategyTimer.print()

    if (Settings.produceHtmlLog)
      Logger_HTML.finish()
  }

  def handleL1() = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 1")

    // add L1 code here

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 1")
  }

  def handleL2() = {
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

  def handleL3() = {
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

  def handleL4() = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 4")

    if (Settings.inputFromJson) {
      StateManager.root_ = (new ParserL4).parseFile(InputReader.layer4)
    } else {
      StateManager.root_ = (new ParserL4).parseFile(Settings.getL4file)
    }

    StateManager.root.asInstanceOf[L4_Root].flatten()

    ValidationL4.apply()

    // re-print the merged L4 state
    if (false) {
      val L4_printed = StateManager.root_.asInstanceOf[L4_Root].prettyprint()

      val outFile = new java.io.FileWriter(Settings.getL4file + "_rep.exa")
      outFile.write((Indenter.addIndentations(L4_printed)))
      outFile.close()

      // re-parse the file to check for errors
      val parserl4 = new ParserL4()
      StateManager.root_ = parserl4.parseFile(Settings.getL4file + "_rep.exa")
      ValidationL4.apply()
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 4")

    L4_UnifyGlobalSections.apply()

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

    if (true) {
      // TODO: optionalize value resolution
      L4_InlineValueDeclarations.apply()
      // resolve globals AFTER L4_InlineValueDeclarations (lower precedence than local values!)
      L4_InlineGlobalValueDeclarations.apply()
    }
    L4_ResolveVirtualFieldAccesses.apply()
    L4_ResolveVariableAccesses.apply()
    L4_ResolveFunctionAccesses.apply()
    L4_ResolveMathFunctions.apply()
    L4_ResolveTimerFunctions.apply()
    L4_ResolveGridFunctions.apply()
    L4_ResolveStencilFunctions.apply()
    L4_ResolveLoopItAccesses.apply()
    L4_ResolveSpecialConstants.apply()
    HACK_L4_ResolveNativeFunctions.apply()
    L4_ResolvePrintFunctions.apply()
    L4_ResolveBuildStringFunctions.apply()
    L4_ResolveKnowledgeParameterAccess.apply()

    L4_ProcessKnowledgeDeclarations.apply()

    if (Knowledge.l4_genSepLayoutsPerField)
      L4_DuplicateFieldLayoutsForFields.apply()

    L4_ResolveFieldAccesses.apply()
    L4_ResolveStencilAccesses.apply()
    L4_ResolveStencilFieldAccesses.apply()

    // after L4_ResolveFieldAccesses
    L4_ResolvePrintFieldFunctions.apply()

    /// BEGIN HACK: progress expression in knowledge
    {
      val oldLoggerLevel = Logger.getLevel
      Logger.setLevel(Logger.WARNING)
      for (obj <- L4_StencilCollection.objects)
        for (entry <- obj.entries) {
          L4_ResolveFieldAccesses.apply(Some(entry))
          L4_ResolveStencilAccesses.apply(Some(entry))
          L4_ResolveStencilFieldAccesses.apply(Some(entry))
        }
      for (obj <- L4_FieldCollection.objects) {
        L4_ResolveFieldAccesses.apply(Some(L4_Root(obj.boundary)))
        L4_ResolveStencilAccesses.apply(Some(L4_Root(obj.boundary)))
        L4_ResolveStencilFieldAccesses.apply(Some(L4_Root(obj.boundary)))
      }
      Logger.setLevel(oldLoggerLevel)
    }
    /// END HACK: progress expression in knowledge

    L4_ResolveBoundaryHandlingFunctions.apply()

    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Progressing from L4 to IR")

    L4_ProgressKnowledge.apply()

    if (Knowledge.data_alignFieldPointers)
      IR_AddPaddingToFieldLayouts.apply()

    StateManager.root_ = StateManager.root_.asInstanceOf[L4_Progressable].progress.asInstanceOf[Node]

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Progressing from L4 to IR")
  }

  def handleIR() = {
    // add some more nodes
    AddDefaultGlobals.apply()
    SetupDataStructures.apply()

    // add remaining nodes
    StateManager.root_.asInstanceOf[IR_Root].nodes ++= List(
      // FunctionCollections
      DomainFunctions(),
      IR_CommunicationFunctions(),

      // Util
      Stopwatch(),
      TimerFunctions(),
      Vector(),
      Matrix(), // TODO: only if required
      CImg() // TODO: only if required
    )

    if (Knowledge.cuda_enabled)
      StateManager.root_.asInstanceOf[IR_Root].nodes += KernelFunctions()

    if (Knowledge.experimental_mergeCommIntoLoops)
      IR_MergeCommunicateAndLoop.apply()
    SimplifyStrategy.doUntilDone() // removes (conditional) calls to communication functions that are not possible
    IR_SetupCommunication.firstCall = true
    IR_SetupCommunication.apply()

    ResolveSpecialFunctionsAndConstants.apply()
    IR_AdaptTimerFunctions.apply()

    ResolveLoopOverPoints.apply()
    ResolveIntergridIndices.apply()

    var convChanged = false
    do {
      FindStencilConvolutions.changed = false
      FindStencilConvolutions.apply()
      convChanged = FindStencilConvolutions.changed
      if (Knowledge.useFasterExpand)
        ExpandOnePassStrategy.apply()
      else
        ExpandStrategy.doUntilDone()
    } while (convChanged)

    ResolveDiagFunction.apply()
    // HACK: create discr_h* again if there are no multigrid level and the field size was defined explicitly
    //   currently this works only if all fields are equally sized
    if (Knowledge.domain_rect_generate && Knowledge.maxLevel <= 0) {
      val fLayout : Array[IR_FieldLayoutPerDim] = IR_FieldCollection.objects.head.fieldLayout.layoutsPerDim
      Knowledge.discr_hx = Array[Double](l3Generate.Domains.getGlobalWidths(0) /
        (Knowledge.domain_rect_numFragsTotal_x * Knowledge.domain_fragmentLength_x * fLayout(0).numInnerLayers))
      if (Knowledge.dimensionality > 1)
        Knowledge.discr_hy = Array[Double](l3Generate.Domains.getGlobalWidths(1) /
          (Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_fragmentLength_y * fLayout(1).numInnerLayers))
      if (Knowledge.dimensionality > 2)
        Knowledge.discr_hz = Array[Double](l3Generate.Domains.getGlobalWidths(2) /
          (Knowledge.domain_rect_numFragsTotal_z * Knowledge.domain_fragmentLength_z * fLayout(2).numInnerLayers))
    }
    Grid.applyStrategies()
    if (Knowledge.domain_fragmentTransformation) CreateGeomCoordinates.apply() // TODO: remove after successful integration

    IR_ResolveLocalSolve.apply()

    ResolveLoopOverPointsInOneFragment.apply()

    IR_SetupCommunication.apply() // handle communication statements generated by loop resolution

    TypeInference.warnMissingDeclarations = false
    TypeInference.apply() // first sweep to allow for VariableAccess extraction in SplitLoopsForHostAndDevice

    if (Knowledge.experimental_memoryDistanceAnalysis) {
      //AnalyzeIterationDistance.apply()
      KernelSubscriptAnalysis.apply()
    }

    if (Knowledge.experimental_kerncraftExport) {
      KerncraftExport.apply()
    }

    if (Knowledge.experimental_addPerformanceEstimate)
      AddPerformanceEstimates.apply()
    // Prepare all suitable LoopOverDimensions and ContractingLoops. This transformation is applied before resolving
    // ContractingLoops to guarantee that memory transfer statements appear only before and after a resolved
    // ContractingLoop (required for temporal blocking). Leads to better device memory occupancy.
    if (Knowledge.cuda_enabled) {
      PrepareCudaRelevantCode.apply()
    }

    ResolveContractingLoop.apply()

    IR_SetupCommunication.apply() // handle communication statements generated by loop resolution

    MapStencilAssignments.apply()
    ResolveFieldAccess.apply()

    if (Knowledge.useFasterExpand)
      ExpandOnePassStrategy.apply()
    else
      ExpandStrategy.doUntilDone()

    // resolve constant IVs before applying poly opt
    ResolveConstInternalVariables.apply()
    SimplifyStrategy.doUntilDone()

    if (Knowledge.opt_conventionalCSE || Knowledge.opt_loopCarriedCSE) {
      new DuplicateNodes().apply() // FIXME: only debug
      Inlining.apply(true)
      CommonSubexpressionElimination.apply()
    }

    MergeConditions.apply()
    if (Knowledge.poly_optLevel_fine > 0)
      PolyOpt.apply()
    ResolveLoopOverDimensions.apply()

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

    LinearizeFieldAccesses.apply() // before converting kernel functions -> requires linearized accesses

    if (Knowledge.cuda_enabled)
      StateManager.findFirst[KernelFunctions]().get.convertToFunctions()

    ResolveBoundedExpressions.apply() // after converting kernel functions -> relies on (unresolved) index offsets to determine loop iteration counts
    ResolveSlotOperationsStrategy.apply() // after converting kernel functions -> relies on (unresolved) slot accesses

    if (Knowledge.useFasterExpand)
      ExpandOnePassStrategy.apply()
    else
      ExpandStrategy.doUntilDone()

    if (!Knowledge.mpi_enabled)
      MPI_RemoveMPI.apply()

    SimplifyStrategy.doUntilDone()

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
      GenerateIndexManipFcts.apply()
    AddInternalVariables.apply()
    // resolve possibly newly added constant IVs
    ResolveConstInternalVariables.apply()

    if (Knowledge.useFasterExpand)
      ExpandOnePassStrategy.apply()
    else
      ExpandStrategy.doUntilDone()

    if (Knowledge.mpi_enabled)
      MPI_AddDatatypeSetup.apply()

    if (Knowledge.omp_enabled) {
      AddOMPPragmas.apply()
    }
    OMP_ResolveParallelSections.apply()
    OMP_ResolveCriticalSections.apply()

    // one last time
    if (Knowledge.useFasterExpand)
      ExpandOnePassStrategy.apply()
    else
      ExpandStrategy.doUntilDone()
    SimplifyStrategy.doUntilDone()

    exastencils.workaround.Compiler.apply()

    if (Knowledge.opt_maxInliningSize > 0)
      Inlining.apply()
    CleanUnusedStuff.apply()

    if (Knowledge.generateFortranInterface)
      Fortranify.apply()
  }

  def print() = {
    Logger.dbg("Prettyprinting to folder " + (new java.io.File(Settings.getOutputPath)).getAbsolutePath)
    PrintStrategy.apply()
    PrettyprintingManager.finish()
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
    (new CountingStrategy("number of printed nodes")).apply()

    shutdown()
  }
}
