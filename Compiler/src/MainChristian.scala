import java.util.Locale

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
import exastencils.parsers.l3._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.util._

object MainChristian {
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

    Platform.compiler = "mpicxx"
    Platform.cflags = "-std=c++11"

    // HACK: this tests the new L4 capabilities
    var parserl3 = new ParserL3
    StateManager.root_ = parserl3.parseFile(Settings.getL3file)
    ValidationL3.apply

    println(StateManager.root_)
    println("Done!")

    println("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
  }
}
