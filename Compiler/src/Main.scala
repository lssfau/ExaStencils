import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ir._
import exastencils.base.l2._
import exastencils.base.l3._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.baseExt.l3._
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
import exastencils.domain.ir._
import exastencils.domain.l2._
import exastencils.domain.l3.L3_DomainCollection
import exastencils.domain.l4.L4_DomainCollection
import exastencils.field.ir._
import exastencils.field.l2._
import exastencils.field.l3._
import exastencils.field.l4._
import exastencils.globals.ir._
import exastencils.grid._
import exastencils.grid.l2._
import exastencils.grid.l3._
import exastencils.grid.l4._
import exastencils.hack.ir.HACK_IR_ResolveSpecialFunctionsAndConstants
import exastencils.hack.l4._
import exastencils.interfacing.ir._
import exastencils.interfacing.l4.L4_ExternalFieldCollection
import exastencils.knowledge.ir.IR_ClearKnowledge
import exastencils.knowledge.l2.L2_KnowledgeContainer._
import exastencils.knowledge.l2._
import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._
import exastencils.logger._
import exastencils.operator.ir.IR_ApplyOffsetToStencilFieldAccess
import exastencils.operator.l2._
import exastencils.operator.l3._
import exastencils.operator.l4._
import exastencils.optimization._
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.optimization.l4.L4_GeneralSimplify
import exastencils.parallelization.api.cuda._
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.api.omp._
import exastencils.parsers.InputReader
import exastencils.parsers.config._
import exastencils.parsers.l2.L2_Parser
import exastencils.parsers.l3.L3_Parser
import exastencils.parsers.l4._
import exastencils.performance._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.solver.ir._
import exastencils.stencil.ir._
import exastencils.timing.ir._
import exastencils.timing.l4.L4_ResolveTimerFunctions
import exastencils.util._
import exastencils.util.l2.L2_ResolveMathFunctions
import exastencils.util.l3.L3_ResolveMathFunctions
import exastencils.util.l4._

object Main {
  private var polyOptExplID : Int = 0

  def initialize(args : Array[String]) = {
    //if (Settings.timeStrategies) -> right now this Schroedinger flag is neither true nor false
    StrategyTimer.startTiming("Initializing")

    StateManager.setRoot(ExaRootNode)

    // check from where to read input
    val settingsParser = new Settings_Parser()
    val knowledgeParser = new Knowledge_Parser()
    val platformParser = new Platform_Parser()
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
      if (args.length >= 4)
        polyOptExplID = args(3).toInt
    }

    // validate knowledge, etc.
    Knowledge.update()
    Settings.update()
    Platform.update()

    // resolve aliases in knowledge, settings and platform
    ResolveAlias.apply()

    if (Settings.cancelIfOutFolderExists) {
      if (new java.io.File(Settings.getOutputPath).exists()) {
        Logger.error(s"Output path ${ Settings.getOutputPath } already exists but cancelIfOutFolderExists is set to true. Shutting down now...")
        sys.exit(0)
      }
    }

    // init buildfile generator, overrides settings file
    if ("MSVC" == Platform.targetCompiler)
      Settings.buildfileGenerators = ListBuffer("ProjectfileGenerator")

    // activate required knowledge collections
    {
      L2_DomainCollection
      L2_FieldCollection
      L2_StencilCollection
      L2_StencilFieldCollection
      L2_VirtualFieldCollection
    }
    {
      L3_DomainCollection
      L3_FieldCollection
      L3_StencilCollection
      L3_StencilFieldCollection
      L3_VirtualFieldCollection
    }
    {
      L4_DomainCollection
      L4_FieldLayoutCollection
      L4_FieldCollection
      L4_StencilCollection
      L4_StencilFieldCollection
      L4_VirtualFieldCollection
      L4_ExternalFieldCollection
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Initializing")
  }

  def shutdown() = {
    StateManager.root.nodes.clear()
    ExaRootNode.clear()
    L2_KnowledgeContainer.clear()
    L3_KnowledgeContainer.clear()
    L4_KnowledgeContainer.clear()
    IR_ClearKnowledge.apply()

    if (Settings.timeStrategies) {
      StrategyTimer.print()
      StrategyTimer.clear()
    }

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
      ExaRootNode.l2_root = L2_Root(Settings.getL2file.map(L2_Parser.parseFile(_) : L2_Node))
      ExaRootNode.l2_root.flatten()

      // pre-process level specifications in declarations
      L2_ResolveLevelSpecifications.apply()

      L2_UnfoldKnowledgeDeclarations.apply()

      // resolve current, etc.
      L2_ResolveRelativeLevels.apply()

      L2_PrepareDeclarations.apply()

      L2_PrepareAccesses.apply()

      L2_ResolveMathFunctions.apply()
      L2_ResolveEvaluateFunctions.apply()
      L2_ResolveIntegrateFunctions.apply()

      var matches = 0
      do {
        matches = 0
        matches += L2_ProcessDeclarations.applyAndCountMatches()
        matches += L2_ResolveAccesses.applyAndCountMatches()
      } while (matches > 0)

      L2_ProcessBoundaryDeclarations.apply()

      if (ExaRootNode.l2_root.nodes.nonEmpty) {
        Logger.warn(s"L2 root has ${ ExaRootNode.l2_root.nodes.length } unprocessed nodes remaining:")
        ExaRootNode.l2_root.nodes.foreach(Logger.warn(_))
      }

      // progress knowledge to L3
      L2_KnowledgeContainer.progress()
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 2")
  }

  def handleL3() = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 3")

    if (Knowledge.experimental_layerExtension) {
      ExaRootNode.l3_root = L3_Root(Settings.getL3file.map(L3_Parser.parseFile(_) : L3_Node))
      ExaRootNode.l3_root.flatten()

      // pre-process level specifications in declarations
      L3_ResolveLevelSpecifications.apply()

      L3_ResolveFunctionInstantiations.apply()
      L3_UnfoldFunctionDeclarations.apply()
      L3_ProcessFunctionDeclarations.apply()

      L3_UnfoldKnowledgeDeclarations.apply()

      // resolve current, etc.
      L3_ResolveRelativeLevels.apply()

      L3_PrepareDeclarations.apply()

      L3_PrepareAccesses.apply()

      L3_ResolveLevelScopes.apply()

      L3_ResolveDslFunctionReferences.apply()
      L3_ResolveMathFunctions.apply()
      L3_ResolveEvaluateFunctions.apply()
      L3_ResolveIntegrateFunctions.apply()

      var matches = 0
      do {
        matches = 0
        matches += L3_ProcessDeclarations.applyAndCountMatches()
        matches += L3_ResolveAccesses.applyAndCountMatches()
      } while (matches > 0)

      L3_ProcessBoundaryDeclarations.apply()
      L3_ProcessFieldOverrides.apply()

      L3_ResolveFieldFieldConvolutions.apply()
      L3_ResolveOperatorTimesField.apply()

      L3_FieldCollection.addInitFieldsFunction()

      // progress knowledge to L4
      L3_KnowledgeContainer.progress()
    } else if (Knowledge.l3tmp_generateL4) {
      val l3gen_root = l3Generate.Root()
      val l4Filenames = Settings.getL4file
      if (l4Filenames.length != 1) Logger.error("l3tmp_generateL4 requires exactly one Layer4 file provided in settings")
      l3gen_root.printToL4(l4Filenames.head)
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 3")
  }

  def handleL4() = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 4")

    // store the l3 root
    val l3root = if (Knowledge.experimental_layerExtension)
      ExaRootNode.l3_root
    else
      null

    if (Settings.inputFromJson)
      ExaRootNode.l4_root = L4_Parser.parseFile(InputReader.layer4)
    else
      ExaRootNode.l4_root = L4_Root(Settings.getL4file.map(L4_Parser.parseFile(_) : L4_Node))
    ExaRootNode.l4_root.flatten()

    L4_Validation.apply()

    if (Knowledge.experimental_layerExtension) {
      // add some extra nodes to test functionalities
      val newL4Root = l3root.progress // progress root
      L4_IntroduceSlots.apply(Some(newL4Root))
      L4_WrapFieldFieldConvolutions.apply(Some(newL4Root))
      L4_AddLoopsToFieldAssignments.apply(Some(newL4Root))
      L4_AddCommunicationToLoops.apply(Some(newL4Root))
      L4_AdaptFieldLayoutsForComm.apply(Some(newL4Root))

      ExaRootNode.l4_root.nodes ++= newL4Root.nodes // TODO: other collections

      if (true) {
        L4_UnresolveOperatorTimesField.apply()
        L4_UnresolveFieldFieldConvolutions.apply()
        L4_UnresolveStencilAccesses.apply()
        L4_UnresolveStencilFieldAccesses.apply()
        L4_UnresolveFieldAccesses.apply()
        // FIXME: transform back to declarations and re-fold
        L4_ReplaceLevelsInFunctionDecls.apply()
        L4_CombineLeveledFunctionDecls.apply()
        // L4_GenerateLeveledKnowledgeDecls.apply()
      }
    }

    // re-print the merged L4 state
    if (Knowledge.experimental_layerExtension) {
      val repFileName = { val tmp = Settings.getL4file.head.split('.'); tmp.dropRight(1).mkString(".") + "_rep." + tmp.last }
      val l4_printed = ExaRootNode.l4_root.prettyprint()

      val outFile = new java.io.FileWriter(repFileName)
      outFile.write(Indenter.addIndentations(l4_printed))
      outFile.close()

      // re-parse the file to check for errors - also clear knowledge collections
      L4_KnowledgeContainer.clear()

      ExaRootNode.l4_root = L4_Parser.parseFile(repFileName)
      L4_Validation.apply()
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 4")

    L4_UnifyGlobalSections.apply()

    // add specialized fields for geometric data - TODO: decide if better left here or moved to ir
    GridGeometry.getGeometry.initL4()

    // go to IR

    // pre-process level specifications in declarations
    L4_ResolveLevelSpecifications.apply()

    L4_ResolveFunctionInstantiations.apply()
    L4_UnfoldFunctionDeclarations.apply()
    L4_ProcessFunctionDeclarations.apply()

    L4_UnfoldLeveledDeclarations.apply()

    L4_UnfoldKnowledgeDeclarations.apply()

    // resolve current, etc.
    L4_ResolveRelativeLevels.apply()

    L4_PrepareDeclarations.apply()

    L4_PrepareAccesses.apply()

    L4_ResolveLevelScopes.apply()

    if (true) {
      // TODO: optionalize value resolution
      L4_InlineValueDeclarations.apply()
      // resolve globals _after_ L4_InlineValueDeclarations (lower precedence than local values!)
      L4_InlineGlobalValueDeclarations.apply()
    }

    L4_ResolveSpecialConstants.apply()
    L4_ResolveDslFunctionReferences.apply()
    L4_ResolveEvaluateFunctions.apply()
    L4_ResolveIntegrateFunctions.apply()
    L4_ResolveMathFunctions.apply()
    L4_ResolveTimerFunctions.apply()

    var matches = 0
    do {
      matches = 0
      matches += L4_ProcessDeclarations.applyAndCountMatches()
      matches += L4_ResolveAccesses.applyAndCountMatches()
    } while (matches > 0)

    if (ExaRootNode.l4_root.nodes.exists(_.isInstanceOf[L4_KnowledgeDecl])) {
      val filtered = ExaRootNode.l4_root.nodes.filter(_.isInstanceOf[L4_KnowledgeDecl])
      Logger.warn(s"L4 root has ${ filtered.length } unprocessed declaration nodes remaining:")
      filtered.foreach(Logger.warn(_))
    }

    L4_ResolveVariableAccesses.apply()
    L4_ResolveStencilFunctions.apply()
    L4_ResolveLoopItAccesses.apply()
    HACK_L4_ResolveNativeFunctions.apply()
    L4_ResolvePrintFunctions.apply()
    L4_ResolveBuildStringFunctions.apply()
    L4_ResolveKnowledgeParameterAccess.apply()

    L4_GeneralSimplify.doUntilDone()

    L4_ProcessKnowledgeDeclarations.apply()

    if (Knowledge.l4_genSepLayoutsPerField)
      L4_DuplicateFieldLayoutsForFields.apply()

    // after L4_ResolveFieldAccesses
    L4_ResolvePrintFieldFunctions.apply()

    L4_ResolveBoundaryHandlingFunctions.apply()

    L4_ResolveStencilComponentAccesses.apply()
    L4_ResolveStencilFieldComponentAccesses.apply()

    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Progressing from L4 to IR")

    // progress knowledge to IR
    L4_KnowledgeContainer.progress()

    //L4_ProgressKnowledge.apply()

    if (Knowledge.data_alignFieldPointers)
      IR_AddPaddingToFieldLayouts.apply()

    ExaRootNode.ProgressToIR()

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
    ExaRootNode.ir_root.nodes ++= List(
      // FunctionCollections
      IR_DomainFunctions(),
      IR_CommunicationFunctions(),

      // Util
      IR_Stopwatch(),
      IR_TimerFunctions(),
      CImg() // TODO: only if required
    )

    if (!Knowledge.experimental_internalHighDimTypes)
      ExaRootNode.ir_root.nodes += IR_Matrix()

    if (Knowledge.cuda_enabled)
      ExaRootNode.ir_root.nodes += CUDA_KernelFunctions()

    if (Knowledge.experimental_mergeCommIntoLoops)
      IR_MergeCommunicateAndLoop.apply()
    IR_GeneralSimplify.doUntilDone() // removes (conditional) calls to communication functions that are not possible
    IR_SetupCommunication.firstCall = true
    IR_SetupCommunication.apply()

    HACK_IR_ResolveSpecialFunctionsAndConstants.apply()
    IR_AdaptTimerFunctions.apply()

    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()

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
    } else {
      def globalSize = IR_DomainCollection.getByIdentifier("global").get.asInstanceOf[IR_DomainFromAABB].aabb

      if (Knowledge.domain_rect_generate) {
        Knowledge.discr_hx = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
          level => globalSize.widthAsDouble(0) / (Knowledge.domain_rect_numFragsTotal_x * Knowledge.domain_fragmentLength_x * (1 << level)))
        if (Knowledge.dimensionality > 1)
          Knowledge.discr_hy = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
            level => globalSize.widthAsDouble(1) / (Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_fragmentLength_y * (1 << level)))
        if (Knowledge.dimensionality > 2)
          Knowledge.discr_hz = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
            level => globalSize.widthAsDouble(2) / (Knowledge.domain_rect_numFragsTotal_z * Knowledge.domain_fragmentLength_z * (1 << level)))
      }
    }
    Grid.applyStrategies()
    if (Knowledge.domain_fragmentTransformation) CreateGeomCoordinates.apply() // TODO: remove after successful integration

    IR_ResolveLoopOverPoints.apply()
    IR_ResolveIntergridIndices.apply()
    IR_ApplyOffsetToFieldAccess.apply()
    IR_ApplyOffsetToStencilFieldAccess.apply()
    // simplify indices modified just now, otherwise equality checks will not work later on
    IR_GeneralSimplify.apply()

    var convChanged = false
    do {
      IR_FindStencilConvolutions.changed = false
      IR_FindStencilConvolutions.apply()
      convChanged = IR_FindStencilConvolutions.changed

      IR_WrapStencilConvolutions.apply()

      if (Knowledge.useFasterExpand)
        IR_ExpandInOnePass.apply()
      else
        IR_Expand.doUntilDone()
    } while (convChanged)

    IR_ResolveStencilFunction.apply()

    // resolve new virtual field accesses
    Grid.applyStrategies()

    IR_ResolveLocalSolve.apply()
    IR_GeneralSimplify.doUntilDone()

    if (Knowledge.experimental_internalHighDimTypes) {
      IR_SetupMatrixExpressions.apply()

      IR_ExtractMatrices.apply()

      // repeat for new VariableAccesses TODO: generate the correct expressions directly
      IR_SetupMatrixExpressions.apply()

      var sthChanged = true
      while (sthChanged) {
        // TODO: move matrix and vector specific parts of IR_GeneralSimplify to specialized strategy
        IR_GeneralSimplify.doUntilDone()

        IR_ResolveMatrixFunctions.apply()
        sthChanged = IR_ResolveMatrixFunctions.results.last._2.matches > 0
      }

      IR_ResolveMatrixAssignments.apply()
      IR_LinearizeMatrices.apply()
    }

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
      KerncraftExportYaml.export()
    }

    if (Knowledge.experimental_addPerformanceEstimate)
      AddPerformanceEstimates.apply()
    // Prepare all suitable LoopOverDimensions and ContractingLoops. This transformation is applied before resolving
    // ContractingLoops to guarantee that memory transfer statements appear only before and after a resolved
    // ContractingLoop (required for temporal blocking). Leads to better device memory occupancy.
    if (Knowledge.cuda_enabled) {
      CUDA_PrepareHostCode.apply()
      CUDA_PrepareMPICode.apply()
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
      DuplicateNodes.instances.clear()
      DuplicateNodes.printStack = false
      DuplicateNodes.apply() // FIXME: only debug
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
      CUDA_AnnotateLoop.apply()
      CUDA_ExtractHostAndDeviceCode.apply()
      CUDA_AdaptKernelDimensionality.apply()
      CUDA_HandleReductions.apply()
    }

    if (Knowledge.opt_useColorSplitting || Knowledge.opt_arrayOfFields)
      LayoutTansformation.apply()

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

    if (Knowledge.experimental_internalHighDimTypes)
      IR_HACK_TypeAliases.apply()
  }

  def print() = {
    Logger.dbg("Prettyprinting to folder " + new java.io.File(Settings.getOutputPath).getAbsolutePath)
    PrintToFile.apply()
    PrettyprintingManager.finish()
  }

  def main(args : Array[String]) : Unit = {
    try {
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
    } catch {
      case e : Throwable =>
        Logger.warn(s"Critical error: ${ e.getMessage }")
        Logger.warn(s"Stack trace:\n${ e.getStackTrace.mkString("\n\tat ") }")

        if (Settings.produceHtmlLog)
          Logger_HTML.finish()

        throw e
    }
  }
}
