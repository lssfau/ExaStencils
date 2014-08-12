import exastencils.communication._
import exastencils.core._
import exastencils.data._
import exastencils.datastructures._
import exastencils.domain._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.languageprocessing.l4.ProgressToIr
import exastencils.mpi._
import exastencils.multiGrid._
import exastencils.omp._
import exastencils.optimization._
import exastencils.parsers.l4._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.util._

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

    SetupDataStructures.apply()

    var numConvFound = 1;
    while (numConvFound > 0) {
      FindStencilConvolutions.apply()
      ResolveSpecialFunctions.apply()
      numConvFound = FindStencilConvolutions.results.last._2.matches
      ExpandStrategy.doUntilDone()
    }

    MapStencilAssignments.apply()

    //////////////////////////////////////////////////////////////////////////////
    //    val test = new java.util.IdentityHashMap[Node, Any]()
    //    new exastencils.datastructures.DefaultStrategy("TestStrategy") {
    //      this += new exastencils.datastructures.Transformation("test", {
    //        case n =>
    //          if (test.containsKey(n))
    //            println("error: " + n.getClass() + "   " + n)
    //          test.put(n, null)
    //          n
    //      })
    //    }.apply()
    //    println("ende...")
    //    return
    //////////////////////////////////////////////////////////////////////////////

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

    if (Knowledge.poly_usePolyOpt)
      PolyOpt.apply()

    ResolveLoopOverDimensions.apply()

    ResolveIndexOffsets.apply()

    LinearizeFieldAccesses.apply()

    ExpandStrategy.doUntilDone()

    if (!Knowledge.useMPI)
      RemoveMPIReferences.apply()

    SimplifyStrategy.doUntilDone()

    if (Knowledge.opt_useAddressPrecalc)
      AddressPrecalculation.apply()

    if (Knowledge.opt_vectorize || Knowledge.opt_unroll > 1)
      TypeInference.apply()

    SimplifyFloatExpressions.apply()

    if (Knowledge.opt_vectorize)
      Vectorization.apply()

    if (Knowledge.opt_unroll > 1)
      Unrolling.apply()

    AddInternalVariables.apply()

    if (Knowledge.useMPI)
      AddMPIDatatypes.apply()

    if (Knowledge.useOMP)
      AddOMPPragmas.apply()

    // one last time
    ExpandStrategy.doUntilDone()
    SimplifyStrategy.doUntilDone()

    PrintStrategy.apply()
    PrettyprintingManager.finish

    println("Done!")

    println("Runtime: " + ((System.nanoTime() - start) / 1e9))
  }
}
