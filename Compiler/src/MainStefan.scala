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

    ResolveContractingLoop.apply()

    MapStencilAssignments.apply()
    if (Knowledge.useFasterExpand)
      ExpandOnePassStrategy.apply()
    else
      ExpandStrategy.doUntilDone()

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //    var doMagic = false
    //    var cp : exastencils.datastructures.ir.LoopOverDimensions = null
    //    val DebugColl = new exastencils.core.collectors.Collector {
    //      def enter(node : exastencils.datastructures.Node) : Unit =
    //        node match {
    //          case f : exastencils.datastructures.ir.FunctionStatement =>
    //            doMagic = f.name == exastencils.datastructures.ir.StringConstant("Smoother_8")
    //          case _ =>
    //        }
    //      def leave(node : exastencils.datastructures.Node) : Unit =
    //        node match {
    //          case f : exastencils.datastructures.ir.FunctionStatement =>
    //            doMagic = false
    //          case _ =>
    //        }
    //      def reset() : Unit = doMagic = false
    //    }
    //    val DebugStrat = new exastencils.datastructures.DefaultStrategy("debug")
    //    DebugStrat += new exastencils.datastructures.Transformation("blub!", {
    //      case l : exastencils.datastructures.ir.LoopOverDimensions if (doMagic) =>
    //        if (cp == null) {
    //          cp = exastencils.core.Duplicate(l)
    //          l
    //        } else
    //          scala.collection.mutable.ListBuffer(cp, l)
    //    })
    //    StateManager.register(DebugColl)
    //    DebugStrat.apply()
    //    StateManager.unregister(DebugColl)
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    MergeConditions.apply()

    if (Knowledge.poly_usePolyOpt)
      PolyOpt.apply()

    ResolveLoopOverDimensions.apply()
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

    TypeInference.apply()

    SimplifyFloatExpressions.apply()
    SimplifyStrategy.doUntilDone()

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
    if (Knowledge.useFasterExpand)
      ExpandOnePassStrategy.apply()
    else
      ExpandStrategy.doUntilDone()
    SimplifyStrategy.doUntilDone()

    PrintStrategy.apply()
    PrettyprintingManager.finish

    println("Done!")

    println("Runtime: " + ((System.nanoTime() - start) / 1e9))
    (new CountingStrategy("number of printed nodes")).apply()
  }
}
