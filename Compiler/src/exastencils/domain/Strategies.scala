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