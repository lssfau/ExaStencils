package exastencils.multiGrid

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.multiGrid._

object GenerateCode_MultiGrid extends (() => Unit) {
  def apply() : Unit = {
    println("Setting up Tree");
    var multiGrid = new MultiGrid;
    StateManager.root_ = Root(List(multiGrid));

    println("Setting up Strategies");
    var strategy = new Strategy("strategy");

    // print
    strategy += new Transformation("Pretty-Print", {
      case printable : FilePrettyPrintable =>
        printable.printToFile;
        Some(printable);
    });

    println("Applying Strategies");
    strategy.apply;
    println("Done");
  }
}