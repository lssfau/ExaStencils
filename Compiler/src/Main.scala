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

object Main {
  def main(args : Array[String]) : Unit = {
    Locale.setDefault(Locale.ENGLISH) // EPIC -.-

    // for runtime measurement
    val start : Long = System.nanoTime()

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

    // Looking for L3 related code? Check MainL3.scala!

    if (Knowledge.l3tmp_generateL4) {
      StateManager.root_ = new l3.Generate.Root
      StateManager.root_.asInstanceOf[l3.Generate.Root].printToL4(Settings.getL4file)
    }

    // read L4
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
    StateManager.root_ = StateManager.root_.asInstanceOf[l4.ProgressableToIr].progressToIr.asInstanceOf[Node]

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

    ResolveSpecialFunctions.apply()

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

    if (!Knowledge.useMPI)
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

    if (Knowledge.useMPI)
      AddMPIDatatypes.apply()

    if (Knowledge.useOMP)
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

    Logger.dbg("Done!")

    Logger.dbg("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
    (new CountingStrategy("number of printed nodes")).apply()

    if (Settings.produceHtmlLog)
      Logger_HTML.finish
  }
}
