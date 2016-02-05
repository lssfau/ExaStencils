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
import exastencils.parsers.l1._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.util._

object MainChristian {
  def main(args : Array[String]) : Unit = {
    // init Settings
    if (args.length >= 1) {
      val s = new exastencils.parsers.settings.ParserSettings
      s.parseFile(args(0))
    }

    // L1
    StateManager.root_ = (new ParserL1).parseFile("/home/schmittch/l1.txt")
    StateManager.root.asInstanceOf[l1.Root].sort()
    exastencils.languageprocessing.l1.UnifyOperators.apply()
    println(StateManager.root)
    sys.exit(0)

  }
}
