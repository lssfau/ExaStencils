package exastencils.domain

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.domain._

object GenerateCode_Domain extends (() => Unit) {
  def apply() : Unit = {
    println("Setting up Tree");
    StateManager.root_ = Root(List(new DomainGenerated));

    println("Setting up Strategies");
    var strategy = new Strategy("strategy");

    // expand applicable nodes - FIXME: do while (changed)
    var expandablesFound = 0;
    strategy += new Transformation("Hoho, expanding all day...", {
      case function : Expandable =>
        expandablesFound += 1;
        Some(function.expand);
    });
    strategy += new Transformation("Hoho, expanding all day...", {
      case function : Expandable =>
        expandablesFound += 1;
        Some(function.expand);
    });

    // print
    strategy += new Transformation("Pretty-Print", {
      case printable : FilePrettyPrintable =>
        printable.printToFile;
        Some(printable);
    });

    println("Applying Strategies");
    strategy.apply;
    println("Done");

    println("Found " + expandablesFound + " Expandable nodes");
  }
}