import java.util.Locale
import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.parsers.l4._
import exastencils.languageprocessing.l4._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._
import exastencils.globals._
import exastencils.strategies._
import exastencils.domain._
import exastencils.multiGrid._
import exastencils.primitives._
import exastencils.util._
import exastencils.mpi._
import exastencils.omp._
import exastencils.spl.FeatureModel
import exastencils.polyhedron._
import exastencils.optimization._

object Main {
  def main(args : Array[String]) : Unit = {
    Locale.setDefault(Locale.ENGLISH) // EPIC -.-

    // for runtime measurement
    val start : Long = System.nanoTime()

    // Init settings

    if (args.length >= 1) {
      val s = new exastencils.parsers.settings.ParserSettings
      s.parseFile(args(0))
    }
    if (args.length >= 2) {
      val k = new exastencils.parsers.settings.ParserKnowledge
      k.parseFile(args(1))
    }

    Knowledge.update()

    // HACK: this will setup a dummy L4 DSL file
    StateManager.root_ = new l3.Root
    StateManager.root_.asInstanceOf[l3.Root].printToL4(Settings.basePathPrefix + "/Compiler/dsl/Layer4.exa")

    // HACK: this tests the new L4 capabilities
    var parserl4 = new ParserL4
    StateManager.root_ = parserl4.parseFile(Settings.basePathPrefix + "/Compiler/dsl/Layer4.exa")
    ValidationL4.apply
    ProgressToIr.apply()

    // TODO: integrate the next line into the ProgressToIr Strategy
    StateManager.root_ = StateManager.root_.asInstanceOf[l4.ProgressableToIr].progressToIr.asInstanceOf[Node]

    // Setup tree
    StateManager.root_.asInstanceOf[ir.Root].nodes ++= List(
      // FunctionCollections
      new DomainFunctions,
      new CommunicationFunctions,

      // Util
      new Log,
      new Stopwatch,
      new Vector)

    // Strategies

    AddDefaultGlobals.apply()

    SetupFragment.apply() // Stefan: This adds the setupBuffer func which will be exapanded using the field info in the next expand step 

    var numConvFound = 1;
    while (numConvFound > 0) {
      FindStencilConvolutions.apply()
      ResolveSpecialFunctions.apply()
      numConvFound = FindStencilConvolutions.results.last._2.matches
      ExpandStrategy.doUntilDone()
    }

    MapStencilAssignments.apply()
    
    if (Knowledge.poly_usePolyOpt)
      PolyOpt.apply()

    ResolveLoopOverDimensions.apply()

    ResolveIndexOffsets.apply()

    LinearizeFieldAccesses.apply()

    if (Knowledge.opt_useAddressPrecalc)
      AddressPrecalculation.apply()

    ExpandStrategy.doUntilDone()

    if (!Knowledge.useMPI)
      RemoveMPIReferences.apply()

    SimplifyStrategy.doUntilDone()

    AddInternalVariables.apply()

    if (Knowledge.useMPI)
      AddMPIDatatypes.apply()

    if (Knowledge.useOMP) {
      AddOMPPragmas.apply()
    }

    // one last time
    ExpandStrategy.doUntilDone()
    SimplifyStrategy.doUntilDone()

    PrintStrategy.apply()
    PrettyprintingManager.finish

    println("Done!")

    println("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
  }
}
