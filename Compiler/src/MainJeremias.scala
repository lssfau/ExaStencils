import java.util.Locale

import exastencils.base.ir.IR_Root
import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.communication._
import exastencils.core._
import exastencils.data._
import exastencils.datastructures._
import exastencils.domain.{ l4 => _, _ }
import exastencils.globals._
import exastencils.knowledge.l4.L4_UnfoldLeveledKnowledgeDecls
import exastencils.knowledge.{ l4 => _, _ }
import exastencils.languageprocessing.l4._
import exastencils.logger._
import exastencils.mpi._
import exastencils.multiGrid._
import exastencils.omp._
import exastencils.optimization._
import exastencils.parsers.l4._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.stencil.l4.L4_ProcessStencilDeclarations
import exastencils.strategies._
import exastencils.util._

object MainJeremias {
  def main(args : Array[String]) : Unit = {

    Locale.setDefault(Locale.ENGLISH) // EPIC -.-

    // for runtime measurement
    val start : Long = System.nanoTime()

    //if (Settings.timeStrategies) -> right now this Schroedinger flag is neither true nor false
    StrategyTimer.startTiming("Initializing")
    println(Knowledge.domain_readFromFile)
    // init Settings
    if (args.length >= 1) {
      val s = new exastencils.parsers.settings.ParserSettings
      s.parseFile(args(0))
    }

    if (Settings.produceHtmlLog)
      Logger_HTML.init

    if (Settings.cancelIfOutFolderExists) {
      if ((new java.io.File(Settings.getOutputPath)).exists) {
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
        val d = new exastencils.parsers.settings.ParserDomainFile
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
        level => l3.Domains.getGlobalWidths(0) / (Knowledge.domain_rect_numFragsTotal_x * Knowledge.domain_fragmentLength_x * (1 << level)))
      if (Knowledge.dimensionality > 1)
        Knowledge.discr_hy = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
          level => l3.Domains.getGlobalWidths(1) / (Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_fragmentLength_y * (1 << level)))
      if (Knowledge.dimensionality > 2)
        Knowledge.discr_hz = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
          level => l3.Domains.getGlobalWidths(2) / (Knowledge.domain_rect_numFragsTotal_z * Knowledge.domain_fragmentLength_z * (1 << level)))
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 2")

    // L3

    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 3")

    // Looking for other L3 related code? Check MainL3.scala!

    if (Knowledge.l3tmp_generateL4) {
      StateManager.root_ = new l3.Generate.Root
      StateManager.root_.asInstanceOf[l3.Generate.Root].printToL4(Settings.getL4file)
    }

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Handling Layer 3")

    // L4

    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Handling Layer 4")

    StateManager.root_ = (new ParserL4).parseFile(Settings.getL4file)
    ValidationL4.apply

    if (false) // re-print the merged L4 state
    {
      val L4_printed = StateManager.root_.asInstanceOf[L4_Root].prettyprint()

      val outFile = new java.io.FileWriter(Settings.getL4file + "_rep.exa")
      outFile.write((Indenter.addIndentations(L4_printed)))
      outFile.close

      // re-parse the file to check for errors
      var parserl4 = new ParserL4
      StateManager.root_ = parserl4.parseFile(Settings.getL4file + "_rep.exa")
      ValidationL4.apply
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

    ResolveL4_Pre.apply()

    L4_ProcessStencilDeclarations.apply()

    ResolveL4_Post.apply()

    StateManager.root_ = StateManager.root_.asInstanceOf[L4_Progressable].progress.asInstanceOf[Node]

    if (!Knowledge.domain_rect_generate) {
      if (Knowledge.domain_readFromFile) {
        val d = new exastencils.parsers.settings.ParserDomainFile
        d.parseBody(args(2))
        DomainCollection.initFragments()
      } else if (Knowledge.domain_onlyRectangular) {
        Knowledge.domain_numBlocks = Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y * Knowledge.domain_rect_numBlocks_z
        Knowledge.domain_numFragmentsPerBlock = Knowledge.domain_rect_numFragsPerBlock_x * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_z
        DomainCollection.initFragments()
      } else if (Knowledge.domain_useCase == "L-Shape") {
        Knowledge.domain_numBlocks = Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y * Knowledge.domain_rect_numBlocks_z
        Knowledge.domain_numFragmentsPerBlock = Knowledge.domain_rect_numFragsPerBlock_x * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_z
        val tmp = Knowledge.mpi_numThreads
        Knowledge.mpi_numThreads = (Knowledge.domain_numBlocks.toDouble - Knowledge.domain_numBlocks.toDouble / 4.0).round.toInt
        Logger.debug("Changed mpi_numThreads (to work with L-shaped Domain) from " + tmp + " to " + Knowledge.mpi_numThreads)
        DomainCollection.initFragments()
        Knowledge.domain_numBlocks = Knowledge.mpi_numThreads
      } else if (Knowledge.domain_useCase == "2-L-Shape") {
        Knowledge.domain_numBlocks = Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y * Knowledge.domain_rect_numBlocks_z
        Knowledge.domain_numFragmentsPerBlock = Knowledge.domain_rect_numFragsPerBlock_x * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_z
        val tmp = Knowledge.mpi_numThreads
        Knowledge.mpi_numThreads = (Knowledge.domain_numBlocks.toDouble - Knowledge.domain_numBlocks.toDouble / 2.0).round.toInt
        Logger.debug("Changed mpi_numThreads (to work with 2-L-shaped Domain) from " + tmp + " to " + Knowledge.mpi_numThreads)
        DomainCollection.initFragments()
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
        DomainCollection.initFragments()
        Knowledge.domain_numBlocks = Knowledge.mpi_numThreads
      }
      if (Knowledge.domain_generateDomainFile) DomainFileWriter.write
    }

    // add remaining nodes
    StateManager.root_.asInstanceOf[IR_Root].nodes ++= List(
      // FunctionCollections
      DomainFunctions(),
      CommunicationFunctions(),

      // Util
      Stopwatch(),
      TimerFunctions(),
      Vector(),
      Matrix())

    // apply strategies

    AddDefaultGlobals.apply()

    SimplifyStrategy.doUntilDone() // removes (conditional) calls to communication functions that are not possible

    SetupDataStructures.apply()
    SetupCommunication.apply()

    ResolveSpecialFunctionsAndConstants.apply()

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
    CreateGeomCoordinates.apply()
    ResolveLoopOverPointsInOneFragment.apply()
    ResolveContractingLoop.apply()

    MapStencilAssignments.apply()
    ResolveFieldAccess.apply()

    if (Knowledge.useFasterExpand)
      ExpandOnePassStrategy.apply()
    else
      ExpandStrategy.doUntilDone()

    MergeConditions.apply()
    if (Knowledge.poly_optLevel_fine > 0)
      PolyOpt.apply()
    ResolveLoopOverDimensions.apply()

    TypeInference.apply()

    if (Knowledge.opt_useColorSplitting)
      ColorSplitting.apply()

    ResolveSlotOperationsStrategy.apply()
    ResolveBoundedExpressions.apply()
    LinearizeFieldAccesses.apply()

    if (Knowledge.useFasterExpand)
      ExpandOnePassStrategy.apply()
    else
      ExpandStrategy.doUntilDone()

    if (!Knowledge.mpi_enabled)
      RemoveMPIReferences.apply()

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

    AddInternalVariables.apply()
    if (Knowledge.useFasterExpand)
      ExpandOnePassStrategy.apply()
    else
      ExpandStrategy.doUntilDone()

    if (Knowledge.mpi_enabled)
      AddMPIDatatypes.apply()

    if (Knowledge.omp_enabled)
      AddOMPPragmas.apply()

    // one last time
    if (Knowledge.useFasterExpand)
      ExpandOnePassStrategy.apply()
    else
      ExpandStrategy.doUntilDone()
    SimplifyStrategy.doUntilDone()

    if (Knowledge.ir_maxInliningSize > 0)
      Inlining.apply()
    CleanUnusedStuff.apply()

    PrintStrategy.apply()
    PrettyprintingManager.finish
    if (!Knowledge.domain_rect_generate) {
      exastencils.domain.FragmentKnowledge.saveFragmentData()
      FragmentCollection.fragments.clear()
    }

    Logger.dbg("Done!")

    Logger.dbg("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
    (new CountingStrategy("number of printed nodes")).apply()

    if (Settings.timeStrategies)
      StrategyTimer.print

    if (Settings.produceHtmlLog)
      Logger_HTML.finish
  }
}
