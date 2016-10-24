import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.l2.L2_ResolveLevelSpecifications
import exastencils.base.l3._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.baseExt.l3.L3_ResolveFunctionTemplates
import exastencils.baseExt.l4._
import exastencils.boundary.ir.L4_ResolveBoundaryHandlingFunctions
import exastencils.communication._
import exastencils.communication.ir._
import exastencils.communication.l4._
import exastencils.config._
import exastencils.core._
import exastencils.core.logger.Logger_HTML
import exastencils.datastructures._
import exastencils.deprecated.ir._
import exastencils.deprecated.l3Generate
import exastencils.domain.ir.IR_DomainFunctions
import exastencils.domain.l2._
import exastencils.domain.l3.L3_DomainCollection
import exastencils.field.ir._
import exastencils.field.l2._
import exastencils.field.l3._
import exastencils.field.l4._
import exastencils.globals.ir._
import exastencils.grid._
import exastencils.grid.l4._
import exastencils.hack.ir.HACK_IR_ResolveSpecialFunctionsAndConstants
import exastencils.hack.l4._
import exastencils.interfacing.ir._
import exastencils.knowledge.l3.L3_FieldCollection
import exastencils.knowledge.l4._
import exastencils.logger._
import exastencils.operator.l2._
import exastencils.operator.l3._
import exastencils.operator.l4._
import exastencils.optimization._
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.parallelization.api.cuda._
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.api.omp._
import exastencils.parsers.InputReader
import exastencils.parsers.l2.L2_Parser
import exastencils.parsers.l3.L3_Parser
import exastencils.parsers.l4._
import exastencils.parsers.settings._
import exastencils.performance._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.solver.ir._
import exastencils.stencil.ir._
import exastencils.stencil.l4._
import exastencils.timing.ir._
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
      if (new java.io.File(Settings.getOutputPath).exists()) {
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

    if (Knowledge.experimental_layerExtension) {
      StateManager.root_ = L2_Parser.parseFile(Settings.getL2file)

      L2_ResolveLevelSpecifications.apply() // before processing declarations ...

      L2_ProcessDomainDeclarations.apply()
      L2_ProcessFieldDeclarations.apply()
      L2_ProcessStencilDeclarations.apply()
      L2_ProcessStencilTemplateDeclarations.apply()

      L2_ResolveLevelSpecifications.apply() // ... and again afterwards

      // progress knowledge to l3
      L2_DomainCollection.progress()
      L2_FieldCollection.progress()
      L2_StencilCollection.progress()
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 2")
  }

  def handleL3() = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 3")

    if (Knowledge.experimental_layerExtension) {
      StateManager.root_ = L3_Parser.parseFile(Settings.getL3file)

      L3_ResolveLevelSpecifications.apply() // before processing declarations ...

      //      L3_ProcessDomainDeclarations.apply()
      L3_ProcessFieldDeclarations.apply()
      L3_ProcessStencilDeclarations.apply()
      L3_ProcessStencilTemplateDeclarations.apply()

      L3_ResolveFunctionTemplates.apply()

      L3_ResolveLevelSpecifications.apply() // ... and again afterwards
      L3_UnfoldFunctionDeclarations.apply()

      L3_ProcessFieldOverrides.apply()

      L3_ResolveFieldAccesses.apply()
      L3_ResolveStencilAccesses.apply()
//      L3_ResolveStencilTemplateAccesses.apply()
      L3_ResolveFieldFieldConvolutions.apply()
      L3_ResolveStencilConvolutions.apply()
//      L3_ResolveStencilTemplateConvolutions.apply()

      L3_FieldCollection.addInitFieldsFunction()

      // progress knowledge to l4
      L3_DomainCollection.progress()
      L3_FieldCollection.prepareFieldLayouts() // prepare field layout knowledge for fields
      //      L3_OperatorCollection.prepareFieldLayouts // prepare  field layout knowledge for stencil fields
      L3_FieldCollection.progress() // progress field knowledge
      L3_StencilCollection.progress() // process stencil knowledge
      //      L3_StencilTemplateCollection.progress // process stencil knowledge
      //      L3_OperatorCollection.progress // process operator knowledge
    } else if (Knowledge.l3tmp_generateL4) {
      StateManager.root_ = l3Generate.Root()
      StateManager.root_.asInstanceOf[l3Generate.Root].printToL4(Settings.getL4file)
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 3")
  }

  def handleL4() = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 4")

    // store the l3 root
    val l3root = if (Knowledge.experimental_layerExtension)
      StateManager.root_.asInstanceOf[L3_Root]
    else
      null

    if (Settings.inputFromJson) {
      StateManager.root_ = (new ParserL4).parseFile(InputReader.layer4)
    } else {
      StateManager.root_ = (new ParserL4).parseFile(Settings.getL4file)
    }

    StateManager.root.asInstanceOf[L4_Root].flatten()

    ValidationL4.apply()

    if (Knowledge.experimental_layerExtension) {
      // add some extra nodes to test functionalities
      val l4root = StateManager.root_.asInstanceOf[L4_Root]

      val newL4Root = l3root.progress // progress root
      L4_IntroduceSlots.apply(Some(newL4Root))
      L4_WrapFieldFieldConvolutions.apply(Some(newL4Root))
      L4_AddLoopsToFieldAssignments.apply(Some(newL4Root))
      L4_AddCommunicationToLoops.apply(Some(newL4Root))
      L4_AdaptFieldLayoutsForComm.apply(Some(newL4Root))

      l4root.nodes ++= newL4Root.nodes // TODO: other collections
    }

    // re-print the merged L4 state
    if (Knowledge.experimental_layerExtension) {
      val repFileName = { val tmp = Settings.getL4file.split('.'); tmp.dropRight(1).mkString(".") + "_rep." + tmp.last }
      val l4_printed = StateManager.root_.asInstanceOf[L4_Root].prettyprint()

      val outFile = new java.io.FileWriter(repFileName)
      outFile.write(Indenter.addIndentations(l4_printed))
      outFile.close()

      // re-parse the file to check for errors - also clear knowledge collections
      L4_ClearKnowledge.apply()

      StateManager.root_ = (new ParserL4).parseFile(repFileName)
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
    IR_AddDefaultGlobals.apply()

    DefaultNeighbors.setup()
    IR_GlobalCollection.get += IR_AllocateDataFunction(IR_FieldCollection.objects, DefaultNeighbors.neighbors)
    IR_ExternalFieldCollection.generateCopyFunction().foreach(IR_UserFunctions.get += _)

    // add remaining nodes
    StateManager.root_.asInstanceOf[IR_Root].nodes ++= List(
      // FunctionCollections
      IR_DomainFunctions(),
      IR_CommunicationFunctions(),

      // Util
      IR_Stopwatch(),
      IR_TimerFunctions(),
      IR_Matrix(), // TODO: only if required
      CImg() // TODO: only if required
    )

    if (Knowledge.cuda_enabled)
      StateManager.root_.asInstanceOf[IR_Root].nodes += CUDA_KernelFunctions()

    if (Knowledge.experimental_mergeCommIntoLoops)
      IR_MergeCommunicateAndLoop.apply()
    IR_GeneralSimplify.doUntilDone() // removes (conditional) calls to communication functions that are not possible
    IR_SetupCommunication.firstCall = true
    IR_SetupCommunication.apply()

    HACK_IR_ResolveSpecialFunctionsAndConstants.apply()
    IR_AdaptTimerFunctions.apply()

    IR_ResolveLoopOverPoints.apply()
    IR_ResolveIntergridIndices.apply()

    var convChanged = false
    do {
      IR_FindStencilConvolutions.changed = false
      IR_FindStencilConvolutions.apply()
      convChanged = IR_FindStencilConvolutions.changed
      if (Knowledge.useFasterExpand)
        IR_ExpandInOnePass.apply()
      else
        IR_Expand.doUntilDone()
    } while (convChanged)

    IR_ResolveStencilFunction.apply()

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

    IR_ResolveLoopOverPointsInOneFragment.apply()

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
      CUDA_PrepareHostCode.apply()
    }

    IR_ResolveContractingLoop.apply()

    IR_SetupCommunication.apply() // handle communication statements generated by loop resolution

    IR_MapStencilAssignments.apply()
    IR_ResolveFieldAccess.apply()

    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()

    IR_ResolveLoopOverFragments.apply()

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
      PolyOpt.apply()
    IR_ResolveLoopOverDimensions.apply()

    TypeInference.apply() // second sweep for any newly introduced nodes - TODO: check if this is necessary

    // Apply CUDA kernel extraction after polyhedral optimizations to work on optimized ForLoopStatements
    if (Knowledge.cuda_enabled) {
      CUDA_AnnotateLoop.apply()
      CUDA_ExtractHostAndDeviceCode.apply()
      CUDA_AdaptKernelDimensionality.apply()
      CUDA_HandleReductions.apply()
    }

    if (Knowledge.opt_useColorSplitting)
      ColorSplitting.apply()

    // before converting kernel functions -> requires linearized accesses
    IR_LinearizeDirectFieldAccess.apply()
    IR_LinearizeExternalFieldAccess.apply()
    IR_LinearizeTempBufferAccess.apply()
    CUDA_LinearizeReductionDeviceDataAccess.apply()
    IR_LinearizeLoopCarriedCSBufferAccess.apply()

    if (Knowledge.cuda_enabled)
      CUDA_KernelFunctions.get.convertToFunctions()

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

    // resolve newly added fragment loops
    IR_ResolveLoopOverFragments.apply()

    if (Knowledge.mpi_enabled) {
      MPI_AddDatatypeSetup.apply()
      MPI_AddReductions.apply()
    }

    if (Knowledge.omp_enabled) {
      OMP_AddParallelSections.apply()

      // resolve min/max reductions for omp versions not supporting them inherently
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

    exastencils.workaround.Compiler.apply()

    if (Knowledge.opt_maxInliningSize > 0)
      Inlining.apply()

    if (Knowledge.generateFortranInterface)
      IR_Fortranify.apply()
  }

  def print() = {
    Logger.dbg("Prettyprinting to folder " + new java.io.File(Settings.getOutputPath).getAbsolutePath)
    PrintToFile.apply()
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
    new CountNodes("number of printed nodes").apply()

    shutdown()
  }
}
