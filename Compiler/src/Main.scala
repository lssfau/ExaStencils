import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.strategies._
import exastencils.application._
import exastencils.domain._
import exastencils.multiGrid._
import exastencils.primitives._
import exastencils.util._
import exastencils.globals._
import exastencils.prettyprinting.PrettyprintingManager

object Main {
  def main(args : Array[String]) : Unit = {
    val s = new exastencils.parsers.settings.ParserSettings
    s.parseFile(args(0))
    val k = new exastencils.parsers.settings.ParserKnowledge
    k.parseFile(args(1))

    Knowledge.update;

    StateManager.root_ = Root(List(
      // Application
      new Poisson3D,

      // MultiGrid
      new MultiGrid,

      // Domain
      new DomainGenerated,

      // Primitives
      new FragmentClass,
      new CommunicationFunctions,
      new FieldCollection,

      // Util
      new Container,
      new Log,
      new Stopwatch,
      new Vector,

      // Globals
      new Globals));

    do { ExpandStrategy.apply; }
    while (ExpandStrategy.results.last._2.replacements > 0) // FIXME: cleaner code

    SetupFragmentClass.apply;
    SetupMultiGrid.apply;
    SetupApplication.apply;

    do { ExpandStrategy.apply; }
    while (ExpandStrategy.results.last._2.replacements > 0) // FIXME: cleaner code

    AddMemberFunctionPrefix.apply;

    AddOMPPragmas.apply;

    PrintStrategy.apply;
    PrettyprintingManager.finish;

    println("Done!");
  }
}
