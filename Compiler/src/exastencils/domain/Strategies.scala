package exastencils.domain

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.domain._

object GenerateCode_Domain extends (() => Unit) {
  def apply() : Unit = {
    StateManager.root_ = Root(List(new DomainGenerated));

    var expandStrategy = new Strategy("Expanding");

    var expandablesFound = 0;
    expandStrategy += new Transformation("Hoho, expanding all day...", {
      case function : Expandable =>
        Some(function.expand);
    });

    do { expandStrategy.apply; }
    while (expandStrategy.results.last._2.replacements > 0) 

    var strategy = new Strategy("strategy");

    // print
    strategy += new Transformation("Pretty-Print", {
      case printable : FilePrettyPrintable =>
        printable.printToFile;
        Some(printable);
    });

    strategy.apply;

    println("Done");
  }
}