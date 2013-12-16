package exastencils.primitives

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._

object GenCommCode extends (() => Unit) {
  def apply() : Unit = {
    var fragmentClass = new FragmentClass;
    var communicationFunctions = new CommunicationFunctions;
    var fieldCollection = new FieldCollection;

    println("Setting up Tree");

    // HACK
    StateManager.root_ = Root(List(fragmentClass, communicationFunctions, fieldCollection));

    println("Setting up Strategies");
    var strategy = new Strategy("strategy");

    strategy += new Transformation("Init FragmentClass", {
      case frag : FragmentClass =>
        frag.init;
        Some(frag);
    });

    strategy += new Transformation("Update FragmentClass with required neighbor declarations", {
      case frag : FragmentClass =>
        for (neigh <- frag.neighbors) { neigh.addDeclarations(frag); }
        Some(frag);
    });

    strategy += new Transformation("Add fields to FragmentClass", {
      case collection : FieldCollection =>
        collection.fields += new Field("Solution", "solData", "double", "NUM_SOL_SLOTS", true);
        collection.fields += new Field("Residual", "resData", "double", "1", false);
        collection.fields += new Field("RHS", "rhsData", "double", "1", false);
        Some(collection);
    });

    strategy += new Transformation("Update FragmentClass with required field declarations", {
      case frag : FragmentClass =>
        for (field <- fieldCollection.fields) {
          frag.declarations += s"ContainerList_1Real ${field.codeName}[${field.numSlots}];";
        }
        Some(frag);
    });

    strategy += new Transformation("Add basic functions to CommunicationFunctions", {
      case commFu : CommunicationFunctions =>
        commFu.functions += new WaitForMPIReq;
        Some(commFu);
    });

    strategy += new Transformation("Add basic functions to FragmentClass", {
      case frag : FragmentClass =>
        frag.functions += new ConnectLocalElement();
        frag.functions += new ConnectRemoteElement();
        frag.functions += new SetupBuffers(fieldCollection.fields);
        Some(frag);
    });

    strategy += new Transformation("Add communication functions to FragmentClass", {
      case commFu : CommunicationFunctions =>
        for (field <- fieldCollection.fields) {

          commFu.functions += new ExchangeDataSplitter(field);
          for (level <- (0 to Knowledge.maxLevel)) {
            commFu.functions += new ExchangeData_6(field, level);
          }
        }
        Some(commFu);
    });

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

    // FIXME: requires nested strategies which currently are not available
    //    strategy += new Transformation("Add function scope prefixes to class member functions", {
    //      case c : Class =>
    //        var strategyAddScopePrefix = new Strategy("strategyAddScopePrefix");
    //        strategyAddScopePrefix += new Transformation({
    //          case function : FunctionStatement =>
    //            println("Found a member function");
    //
    //            // add to name
    //
    //            Some(function);
    //        }, true, c)
    //
    //        strategyAddScopePrefix.apply;
    //
    //        Some(c);
    //    });
    strategy += new Transformation("Add function scope prefixes to class member functions", {
      case c : Class =>
        for (func <- c.functions) {
          func match { case f : FunctionStatement => f.name = s"${c.className}::${f.name}"; }
        }
        Some(c);
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