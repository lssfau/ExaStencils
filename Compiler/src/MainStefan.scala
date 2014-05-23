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
import exastencils.application._
import exastencils.domain._
import exastencils.multiGrid._
import exastencils.primitives._
import exastencils.util._
import exastencils.mpi._
import exastencils.omp._
import exastencils.spl.FeatureModel
import exastencils.polyhedron._

object MainStefan {
  def main(args : Array[String]) : Unit = {

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

    // HACK: this tests the new L4 capabilities
    var parserl4 = new ParserL4
    StateManager.root_ = parserl4.parseFile(Settings.basePathPrefix + "/Compiler/dsl/newDSL4.exa")
    ValidationL4.apply
    ProgressToIr.apply()

    StateManager.root_ = StateManager.root_.asInstanceOf[l4.ProgressableToIr].progressToIr.asInstanceOf[Node]

    // Setup tree
    StateManager.root_.asInstanceOf[ir.Root].nodes ++= List(
      // Application
      new Poisson3D,

      // Domain
      new DomainGenerated,

      // Primitives
      new FragmentClass,
      new CommunicationFunctions,

      // Util
      new Log,
      new Stopwatch,
      new Vector,

      // Globals
      new Globals)

    // Strategies

    FindStencilConvolutions.apply()

    ResolveSpecialFunctions.apply()

    SetupFragmentClass.apply()

    ExpandStrategy.doUntilDone()

    PolyOpt.apply()

    ResolveLoopOverDimensions.apply()

    LinearizeFieldAccesses.apply()

    ExpandStrategy.doUntilDone()

    if (!Knowledge.useMPI) {
      RemoveMPIReferences.apply()
    }

    SimplifyStrategy.doUntilDone()

    AddMemberFunctionPrefix.apply()

    if (Knowledge.useOMP) {
      AddOMPPragmas.apply()
    }

    PrintStrategy.apply()
    PrettyprintingManager.finish

    println("Done!")

    println("Runtime: " + ((System.nanoTime() - start) / 1e9))
  }
}
