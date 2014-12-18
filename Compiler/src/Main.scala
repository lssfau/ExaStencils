import java.util.Locale

import exastencils.communication._
import exastencils.core._
import exastencils.data._
import exastencils.datastructures._
import exastencils.domain._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.languageprocessing.l4._
import exastencils.mpi._
import exastencils.multiGrid._
import exastencils.omp._
import exastencils.optimization._
import exastencils.parsers.l3._
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

    if (Settings.cancelIfOutFolderExists) {
      if ((new java.io.File(Settings.outputPath)).exists) {
        exastencils.core.Logger.error(s"Output path ${Settings.outputPath} already exists but cancelIfOutFolderExists is set to true. Shutting down now...")
        sys.exit(0)
      }
    }

    // init Knowledge
    if (args.length >= 2) {
      val k = new exastencils.parsers.settings.ParserKnowledge
      k.parseFile(args(1))
    }
    Knowledge.update()

    // read L3    
    StateManager.root_ = (new ParserL3).parseFile(Settings.getL3file)
    ValidationL3.apply

    // progress L3 to L4
    StateManager.root_ = StateManager.root_.asInstanceOf[l3.Root].progressToL4

    // BEGIN HACK: add function to test new functionalities
    val statements : List[l4.Statement] = List(
      l4.AssignmentStatement(
        l4.FieldAccess("Residual", l4.CurrentLevelSpecification(), l4.IntegerConstant(0), -1),
        l4.BinaryExpression("-",
          l4.FieldAccess("RHS", l4.CurrentLevelSpecification(), l4.IntegerConstant(0), -1),
          l4.StencilConvolution(
            l4.StencilAccess("Laplace", l4.CurrentLevelSpecification()),
            l4.FieldAccess("Solution", l4.CurrentLevelSpecification(), l4.BasicIdentifier("curSlot"), -1))),
        "="))

    // version without comm
    StateManager.root.asInstanceOf[l4.Root].statements += new l4.FunctionStatement(
      l4.LeveledIdentifier("UpResidual", l4.AllLevelsSpecification()),
      l4.UnitDatatype(),
      List(),
      statements)

    // version with comm
    //    StateManager.root.asInstanceOf[l4.Root].statements += new l4.FunctionStatement(
    //      l4.LeveledIdentifier("UpResidual", l4.AllLevelsSpecification()),
    //      l4.UnitDatatype(),
    //      List(),
    //      List[l4.Statement](
    //        l4.CommunicateStatement(
    //          l4.FieldAccess("Solution", l4.CurrentLevelSpecification(), l4.BasicIdentifier("curSlot"), -1),
    //          "both", List()),
    //        l4.LoopOverFragmentsStatement(List(
    //          l4.LoopOverPointsStatement(
    //            l4.FieldAccess("Residual", l4.CurrentLevelSpecification(), l4.IntegerConstant(0), -1),
    //            false, None, None, None, None, statements, None)),
    //          None)))

    // add loops and communication
    WrapL4FieldOpsStrategy.apply()

    // END HACK

    if (Knowledge.l3tmp_generateL4) {
      // print current L4 state to string
      val l4_from_l3 = new PpStream()
      StateManager.root_.asInstanceOf[l4.Root].prettyprint(l4_from_l3)

      // generate other half of l4
      StateManager.root_ = new l3.Generate.Root
      // print to file
      StateManager.root_.asInstanceOf[l3.Generate.Root].printToL4(Settings.getL4file)

      // add parts coming from L3 to the new L4 file
      val outFile = new java.io.FileWriter(Settings.getL4file, true)
      outFile.write((Indenter.addIndentations(l4_from_l3.toString)))
      outFile.close
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

    var numConvFound = 1
    while (numConvFound > 0) {
      FindStencilConvolutions.apply()
      numConvFound = FindStencilConvolutions.results.last._2.matches
      if (Knowledge.useFasterExpand)
        ExpandOnePassStrategy.apply()
      else
        ExpandStrategy.doUntilDone()
    }

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

    PrintStrategy.apply()
    PrettyprintingManager.finish

    println("Done!")

    println("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
    (new CountingStrategy("number of printed nodes")).apply()
  }
}
