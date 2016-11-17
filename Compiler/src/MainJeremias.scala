import java.util.Locale

import exastencils.base.ExaRootNode
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.baseExt.l4._
import exastencils.communication._
import exastencils.communication.ir._
import exastencils.config._
import exastencils.core._
import exastencils.core.logger.Logger_HTML
import exastencils.datastructures._
import exastencils.deprecated._
import exastencils.deprecated.domain._
import exastencils.deprecated.ir._
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.globals.ir._
import exastencils.hack.ir.HACK_IR_ResolveSpecialFunctionsAndConstants
import exastencils.interfacing.ir._
import exastencils.knowledge.l4._
import exastencils.logger._
import exastencils.operator.l4.L4_ProcessStencilDeclarations
import exastencils.optimization._
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.parallelization.api.cuda.CUDA_LinearizeReductionDeviceDataAccess
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.api.omp._
import exastencils.parsers.l4._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.solver.ir.IR_ResolveIntergridIndices
import exastencils.stencil.ir._
import exastencils.timing.ir._
import exastencils.util._
import exastencils.util.l4.L4_ResolveSpecialConstants

object MainJeremias {
  def main(args : Array[String]) : Unit = {

    Locale.setDefault(Locale.ENGLISH) // EPIC -.-

    // for runtime measurement
    val start : Long = System.nanoTime()

    StateManager.setRoot(ExaRootNode)

    //if (Settings.timeStrategies) -> right now this Schroedinger flag is neither true nor false
    StrategyTimer.startTiming("Initializing")
    println(Knowledge.domain_readFromFile)
    // init Settings
    if (args.length >= 1) {
      val s = new exastencils.parsers.settings.ParserSettings
      s.parseFile(args(0))
    }

    if (Settings.produceHtmlLog)
      Logger_HTML.init()

    if (Settings.cancelIfOutFolderExists) {
      if (new java.io.File(Settings.getOutputPath).exists) {
        Logger.error(s"Output path ${ Settings.getOutputPath } already exists but cancelIfOutFolderExists is set to true. Shutting down now...")
        sys.exit(0)
      }
    }

    // init Knowledge
    if (args.length >= 2) {
      val k = new exastencils.parsers.settings.ParserKnowledge
      k.parseFile(args(1))
    }

    Knowledge.update()

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Initializing")

    // L1

    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 1")

    if (Knowledge.domain_readFromFile) {
      if (args.length >= 3) {
        val d = new ParserDomainFile
        d.parseHeader(args(2))
        DomainFileHeader.updateKnowledge()
      } else Logger.error("No file for domain configuration has been commited as third argument")
    }
    // add L1 code here

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 1")

    // L2

    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 2")

    /// HACK: This information has to come from L2
    if (Knowledge.domain_rect_generate || Knowledge.domain_useCase != "" || Knowledge.domain_onlyRectangular) {
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

    // L3

    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 3")

    // Looking for other L3 related code? Check MainL3.scala!

    if (Knowledge.l3tmp_generateL4) {
      var l3gen_root = l3Generate.Root()
      l3gen_root.printToL4(Settings.getL4file)
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 3")

    // L4

    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 4")

    ExaRootNode.l4_root = (new ParserL4).parseFile(Settings.getL4file)
    ValidationL4.apply()

    if (false) // re-print the merged L4 state
    {
      val L4_printed = ExaRootNode.l4_root.prettyprint()

      val outFile = new java.io.FileWriter(Settings.getL4file + "_rep.exa")
      outFile.write(Indenter.addIndentations(L4_printed))
      outFile.close()

      // re-parse the file to check for errors
      var parserl4 = new ParserL4
      ExaRootNode.l4_root = parserl4.parseFile(Settings.getL4file + "_rep.exa")
      ValidationL4.apply()
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 4")

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

    ExaRootNode.ProgressToIR()

    if (!Knowledge.domain_rect_generate) {
      if (Knowledge.domain_readFromFile) {
        val d = new ParserDomainFile
        d.parseBody(args(2))
        IR_DomainCollection.initFragments()
      } else if (Knowledge.domain_onlyRectangular) {
        Knowledge.domain_numBlocks = Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y * Knowledge.domain_rect_numBlocks_z
        Knowledge.domain_numFragmentsPerBlock = Knowledge.domain_rect_numFragsPerBlock_x * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_z
        IR_DomainCollection.initFragments()
      } else if (Knowledge.domain_useCase == "L-Shape") {
        Knowledge.domain_numBlocks = Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y * Knowledge.domain_rect_numBlocks_z
        Knowledge.domain_numFragmentsPerBlock = Knowledge.domain_rect_numFragsPerBlock_x * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_z
        val tmp = Knowledge.mpi_numThreads
        Knowledge.mpi_numThreads = (Knowledge.domain_numBlocks.toDouble - Knowledge.domain_numBlocks.toDouble / 4.0).round.toInt
        Logger.debug("Changed mpi_numThreads (to work with L-shaped Domain) from " + tmp + " to " + Knowledge.mpi_numThreads)
        IR_DomainCollection.initFragments()
        Knowledge.domain_numBlocks = Knowledge.mpi_numThreads
      } else if (Knowledge.domain_useCase == "2-L-Shape") {
        Knowledge.domain_numBlocks = Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y * Knowledge.domain_rect_numBlocks_z
        Knowledge.domain_numFragmentsPerBlock = Knowledge.domain_rect_numFragsPerBlock_x * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_z
        val tmp = Knowledge.mpi_numThreads
        Knowledge.mpi_numThreads = (Knowledge.domain_numBlocks.toDouble - Knowledge.domain_numBlocks.toDouble / 2.0).round.toInt
        Logger.debug("Changed mpi_numThreads (to work with 2-L-shaped Domain) from " + tmp + " to " + Knowledge.mpi_numThreads)
        IR_DomainCollection.initFragments()
        Knowledge.domain_numBlocks = Knowledge.mpi_numThreads
      } else if (Knowledge.domain_useCase == "X-Shape") {
        Knowledge.domain_rect_numBlocks_x = 2
        Knowledge.domain_rect_numBlocks_y = 2
        Knowledge.domain_rect_numBlocks_z = 1

        Knowledge.domain_numBlocks = Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y * Knowledge.domain_rect_numBlocks_z

        Knowledge.domain_rect_numFragsPerBlock_x = 2
        Knowledge.domain_rect_numFragsPerBlock_y = 2
        Knowledge.domain_rect_numFragsPerBlock_z = 1
        Knowledge.domain_numFragmentsPerBlock = Knowledge.domain_rect_numFragsPerBlock_x * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_z
        Knowledge.mpi_numThreads = 4
        IR_DomainCollection.initFragments()
        Knowledge.domain_numBlocks = Knowledge.mpi_numThreads
      }
      if (Knowledge.domain_generateDomainFile) DomainFileWriter.write()
    }

    // add remaining nodes
    ExaRootNode.ir_root.nodes ++= List(
      // FunctionCollections
      IR_DomainFunctions(),
      IR_CommunicationFunctions(),

      // Util
      IR_Stopwatch(),
      IR_TimerFunctions(),
      IR_Matrix())

    // apply strategies

    IR_AddDefaultGlobals.apply()

    IR_GeneralSimplify.doUntilDone() // removes (conditional) calls to communication functions that are not possible

    DefaultNeighbors.setup()
    IR_GlobalCollection.get += IR_AllocateDataFunction(IR_FieldCollection.objects, DefaultNeighbors.neighbors)
    IR_ExternalFieldCollection.generateCopyFunction().foreach(IR_UserFunctions.get += _)

    IR_SetupCommunication.apply()

    HACK_IR_ResolveSpecialFunctionsAndConstants.apply()

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
    CreateGeomCoordinates.apply()
    IR_ResolveLoopOverPointsInOneFragment.apply()
    IR_ResolveContractingLoop.apply()

    IR_MapStencilAssignments.apply()
    IR_ResolveFieldAccess.apply()

    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply()
    else
      IR_Expand.doUntilDone()

    MergeConditions.apply()
    if (Knowledge.poly_optLevel_fine > 0)
      PolyOpt.apply()
    IR_ResolveLoopOverDimensions.apply()

    TypeInference.apply()

    if (Knowledge.opt_useColorSplitting)
      ColorSplitting.apply()

    IR_ResolveSlotOperations.apply()
    IR_ResolveBoundedScalar.apply()

    // before converting kernel functions -> requires linearized accesses
    IR_LinearizeDirectFieldAccess.apply()
    IR_LinearizeExternalFieldAccess.apply()
    IR_LinearizeTempBufferAccess.apply()
    CUDA_LinearizeReductionDeviceDataAccess.apply()
    IR_LinearizeLoopCarriedCSBufferAccess.apply()

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

    IR_AddInternalVariables.apply()
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

    PrintToFile.apply()
    PrettyprintingManager.finish()
    if (!Knowledge.domain_rect_generate) {
      FragmentKnowledge.saveFragmentData()
      FragmentCollection.fragments.clear()
    }

    Logger.dbg("Done!")

    Logger.dbg("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
    new CountNodes("number of printed nodes").apply()

    if (Settings.timeStrategies)
      StrategyTimer.print()

    if (Settings.produceHtmlLog)
      Logger_HTML.finish()
  }
}
