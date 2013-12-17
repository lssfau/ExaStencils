package exastencils.application

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.application._

object GenerateCode_Application extends (() => Unit) {
  def apply() : Unit = {
    println("Setting up Tree");
    StateManager.root_ = Root(List(new Poisson3D));

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