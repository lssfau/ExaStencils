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

object Main {
  def main(args : Array[String]) : Unit = {
    Globals.printPath = s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Generated/";

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
      new Vector));

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

    println("Done!");
  }
}
