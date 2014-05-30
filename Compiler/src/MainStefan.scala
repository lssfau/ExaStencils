import exastencils.application.Poisson3D
import exastencils.core.Settings
import exastencils.core.StateManager
import exastencils.datastructures.Node
import exastencils.datastructures.ir
import exastencils.datastructures.l4
import exastencils.domain.DomainGenerated
import exastencils.globals.Globals
import exastencils.knowledge.FindStencilConvolutions
import exastencils.knowledge.Knowledge
import exastencils.languageprocessing.l4.ProgressToIr
import exastencils.mpi.RemoveMPIReferences
import exastencils.multiGrid.ResolveSpecialFunctions
import exastencils.omp.AddOMPPragmas
import exastencils.parsers.l4.ParserL4
import exastencils.parsers.l4.ValidationL4
import exastencils.polyhedron.PolyOpt
import exastencils.prettyprinting.PrettyprintingManager
import exastencils.primitives.CommunicationFunctions
import exastencils.primitives.FragmentClass
import exastencils.primitives.LinearizeFieldAccesses
import exastencils.primitives.ResolveIndexOffsets
import exastencils.primitives.ResolveLoopOverDimensions
import exastencils.primitives.SetupFragmentClass
import exastencils.strategies.AddMemberFunctionPrefix
import exastencils.strategies.ExpandStrategy
import exastencils.strategies.PrintStrategy
import exastencils.strategies.SimplifyStrategy
import exastencils.util.Log
import exastencils.util.Stopwatch
import exastencils.util.Vector

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

    //    new DefaultStrategy("TestStrategy") {
    //      this += new Transformation("test", {
    //        case acc : DirectFieldAccess =>
    //          println("DirectFieldAccess")
    //          println(acc.field.identifier + " " + acc.field.level)
    //          println(acc.field.codeName.cpp)
    //          println()
    //          acc
    //        case acc : FieldAccess =>
    //          println("FieldAccess")
    //          println(acc.field.identifier + " " + acc.field.level)
    //          println(acc.field.codeName.cpp)
    //          println()
    //          acc
    //        case acc : ExternalFieldAccess =>
    //          println("ExternalFieldAccess")
    //          println(acc.field.identifier + " " + acc.field.level)
    //          println(acc.field.targetFieldIdentifier)
    //          println()
    //          acc
    //      })
    //    }.apply()
    //    return

    PolyOpt.apply()

    ResolveLoopOverDimensions.apply()

    ResolveIndexOffsets.apply()

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
