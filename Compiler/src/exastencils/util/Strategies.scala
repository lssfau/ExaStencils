package exastencils.util

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.util._

object GenerateCode_Util extends (() => Unit) {
  def apply() : Unit = {
    println("Setting up Tree");
    StateManager.root_ = Root(List(new Container, new Defines, new Log, new StdIncludes, new Stopwatch, new TypeDefs, new Vector));

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