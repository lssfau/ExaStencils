import java.util.Locale

import exastencils.communication._
import exastencils.core._
import exastencils.data._
import exastencils.datastructures._
import exastencils.domain._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.languageprocessing.l4._
import exastencils.logger._
import exastencils.mpi._
import exastencils.multiGrid._
import exastencils.omp._
import exastencils.optimization._
import exastencils.parsers.l4._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.util._
import jeremias.dsl._

import scala.collection.mutable._

object MainJeremias {
  def main(args: Array[String]): Unit = {
    Locale.setDefault(Locale.ENGLISH) // EPIC -.-

    // for runtime measurement
    val start: Long = System.nanoTime()

    // init Settings
    if (args.length >= 1) {
      val s = new exastencils.parsers.settings.ParserSettings
      s.parseFile(args(0))
    }

    if (Settings.produceHtmlLog)
      Logger_HTML.init

    if (Settings.cancelIfOutFolderExists) {
      if ((new java.io.File(Settings.outputPath)).exists) {
        Logger.error(s"Output path ${Settings.outputPath} already exists but cancelIfOutFolderExists is set to true. Shutting down now...")
        sys.exit(0)
      }
    }

    // init Knowledge
    if (args.length >= 2) {
      val k = new exastencils.parsers.settings.ParserKnowledge
      k.parseFile(args(1))
    }
    Knowledge.update()

    // L1

    // L2

    /// HACK: This information has to come from L2
    if (Knowledge.domain_rect_generate) {
      Knowledge.discr_hx = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
        level => l3.Domains.getGlobalWidths(0) / (Knowledge.domain_rect_numFragsTotal_x * Knowledge.domain_fragmentLength_x * (1 << level)))
      if (Knowledge.dimensionality > 1)
        Knowledge.discr_hy = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
          level => l3.Domains.getGlobalWidths(1) / (Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_fragmentLength_y * (1 << level)))
      if (Knowledge.dimensionality > 2)
        Knowledge.discr_hz = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
          level => l3.Domains.getGlobalWidths(2) / (Knowledge.domain_rect_numFragsTotal_z * Knowledge.domain_fragmentLength_z * (1 << level)))
    }

    // L3

    // Looking for L3 related code? Check MainL3.scala!

    if (Knowledge.l3tmp_generateL4) {
      StateManager.root_ = new l3.Generate.Root
      StateManager.root_.asInstanceOf[l3.Generate.Root].printToL4(Settings.getL4file)
    }

    // L4

    StateManager.root_ = (new ParserL4).parseFile(Settings.getL4file)

    ValidationL4.apply

    if (false) // re-print the merged L4 state
    {
      val l4_printed = new PpStream()
      StateManager.root_.asInstanceOf[l4.Root].prettyprint(l4_printed)

      val outFile = new java.io.FileWriter(Settings.getL4file + "_rep.exa")
      outFile.write((Indenter.addIndentations(l4_printed.toString)))
      outFile.close

      // re-parse the file to check for errors
      var parserl4 = new ParserL4
      StateManager.root_ = parserl4.parseFile(Settings.getL4file + "_rep.exa")
      ValidationL4.apply
    }

    // go to IR
    ProgressToIr.apply() // preparation step
    ResolveL4Constants.apply()
    StateManager.root_ = StateManager.root_.asInstanceOf[l4.ProgressableToIr].progressToIr.asInstanceOf[Node]

    if (Knowledge.domain_formUnion && Knowledge.mpi_enabled) { // for L-shape domain
      Knowledge.mpi_numThreads = Knowledge.domain_numBlocks * DomainCollection.domains.count { d => d.identifier == "global" || d.identifier.contains("union_") }
    }

    // add remaining nodes
    StateManager.root_.asInstanceOf[ir.Root].nodes ++= List(
      // FunctionCollections
      new DomainFunctions,
      new CommunicationFunctions,

      // Util
      new Stopwatch,
      new Vector)

    // apply strategies

    AddDefaultGlobals.apply()

    SimplifyStrategy.doUntilDone() // removes (conditional) calls to communication functions that are not possible

    SetupDataStructures.apply()
    SetupCommunication.apply()

    ResolveSpecialFunctionsAndConstants.apply()

    ResolveLoopOverPoints.apply()
    ResolveIntergridIndices.apply()

    CreateGeomCoordinates.apply()

    var numConvFound = 0
    do {
      FindStencilConvolutions.apply()
      numConvFound = FindStencilConvolutions.results.last._2.matches
      if (Knowledge.useFasterExpand)
        ExpandOnePassStrategy.apply()
      else
        ExpandStrategy.doUntilDone()
    } while (numConvFound > 0)

    ResolveDiagFunction.apply()
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
    ResolveIndexOffsets.apply()
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

    CleanUnusedStuff.apply()

    PrintStrategy.apply()
    PrettyprintingManager.finish

    if (!Knowledge.domain_rect_generate) {
      DomainCollection.initFragments()
      if (Knowledge.domain_formUnion) {
        //connect domains to one l-shape domain
        val v1_global_right = new Vertex(ListBuffer(0.5, 0.0))
        val v2_global_right = new Vertex(ListBuffer(0.5, 0.5))
        val e_global_right = new Edge(v1_global_right, v2_global_right)
        DomainCollection.unifyDomains("global", "union_right", new Face(ListBuffer(e_global_right), ListBuffer(v1_global_right, v2_global_right)))

        val v1_global_top = new Vertex(ListBuffer(0, 0.5))
        val v2_global_top = new Vertex(ListBuffer(0.5, 0.5))
        val e_global_top = new Edge(v1_global_top, v2_global_top)
        DomainCollection.unifyDomains("global", "union_top", new Face(ListBuffer(e_global_top), ListBuffer(v1_global_top, v2_global_top)))

      }
      jeremias.dsl.FragmentKnowledge.saveFragmentData()
    }

    Logger.dbg("Done!")

    Logger.dbg("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
    (new CountingStrategy("number of printed nodes")).apply()

    if (Settings.produceHtmlLog)
      Logger_HTML.finish
  }
}
